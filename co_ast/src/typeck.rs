use super::*;
use co_diag::CompileError;
use resolve::Res;
use std::collections::HashMap;
use std::fmt;
use tindex::{TIndex, TVec};
use visit::{Visitor, Walkable};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct InferTy(usize);
impl From<usize> for InferTy {
    fn from(v: usize) -> InferTy {
        InferTy(v)
    }
}
impl TIndex for InferTy {
    fn as_index(&self) -> usize {
        self.0
    }
}
impl fmt::Display for InferTy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "?{}", self.0)
    }
}

#[derive(Debug, Clone)]
pub enum Ty {
    Infer(InferTy),
    Bound(usize),
    Fn(Box<Ty>, Box<Ty>),
}

impl fmt::Display for Ty {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Ty::Infer(ty) => write!(f, "{}", ty),
            Ty::Bound(b) => write!(f, "#{}", b),
            Ty::Fn(arg, ret) => write!(f, "{} -> {}", arg, ret),
        }
    }
}

#[derive(Debug, Clone)]
enum Infer {
    Known(Ty),
    Eq(InferTy),
    Unknown,
}

struct TypeckCtxt<'a> {
    module: &'a Module<'a>,
    bound_var_map: HashMap<AstId, usize>,
    res: &'a HashMap<AstId, Res>,
    inference_vars: TVec<InferTy, Infer>,
    types: HashMap<AstId, Ty>,
}

impl<'a> TypeckCtxt<'a> {
    fn ast_ty_to_ty(&mut self, ast_ty: &Type) -> Result<Ty, CompileError> {
        match ast_ty {
            Type::Func(arg, ret) => {
                let arg = self.ast_ty_to_ty(arg)?;
                let ret = self.ast_ty_to_ty(ret)?;
                Ok(Ty::Fn(Box::new(arg), Box::new(ret)))
            }
            Type::Path(path) => match self.res.get(&path.id).unwrap() {
                Res::Def(_) => todo!(),
                Res::BoundVar(b) => Ok(Ty::Bound(self.bound_var_map.get(b).unwrap().clone())),
                Res::LambdaArg(arg) => unreachable!(),
            },
        }
    }

    fn infer_ty(&mut self) -> Ty {
        Ty::Infer(self.inference_vars.push(Infer::Unknown))
    }

    fn deep_resolve(&mut self, t: Ty) -> Ty {
        let ty = self.shallow_resolve(t);
        match ty {
            Ty::Infer(_) | Ty::Bound(_) => ty,
            Ty::Fn(arg, ret) => Ty::Fn(
                Box::new(self.deep_resolve(*arg)),
                Box::new(self.deep_resolve(*ret)),
            ),
        }
    }

    fn shallow_resolve(&mut self, t: Ty) -> Ty {
        if let Ty::Infer(infer) = t {
            match self.inference_vars[infer].clone() {
                Infer::Known(ty) => ty,
                Infer::Eq(inf) => self.shallow_resolve(Ty::Infer(inf)),
                Infer::Unknown => t,
            }
        } else {
            t
        }
    }

    fn eq_infer(&mut self, sp: Span<'a>, inf: InferTy, ty: Ty) -> Result<Ty, CompileError> {
        if let Ty::Infer(other) = ty {
            if inf != other {
                self.inference_vars[inf] = Infer::Eq(other);
            }

            return Ok(ty);
        }

        match self.inner_eq_infer(inf, ty.clone()) {
            Ok(ty) => {
                match ty {
                    Ty::Infer(_) => unreachable!(),
                    _ => self.inference_vars[inf] = Infer::Known(ty.clone()),
                }

                Ok(ty)
            }
            Err(()) => CompileError::new(
                sp,
                format!("cyclic type: trying to unify `{}` with `{}`", inf, ty),
            )?,
        }
    }

    fn inner_eq_infer(&mut self, inf: InferTy, ty: Ty) -> Result<Ty, ()> {
        let ty = self.shallow_resolve(ty);
        match ty {
            Ty::Infer(other) => {
                if other == inf {
                    Err(())
                } else {
                    Ok(ty)
                }
            }
            Ty::Bound(_) => Ok(ty),
            Ty::Fn(arg, ret) => Ok(Ty::Fn(
                Box::new(self.inner_eq_infer(inf, *arg)?),
                Box::new(self.inner_eq_infer(inf, *ret)?),
            )),
        }
    }

    fn eq_tys(&mut self, sp: Span<'a>, a: Ty, b: Ty) -> Result<Ty, CompileError> {
        let a = self.shallow_resolve(a);
        let b = self.shallow_resolve(b);

        match (&a, &b) {
            (&Ty::Infer(infer), other) | (other, &Ty::Infer(infer)) => {
                self.eq_infer(sp, infer, other.clone())
            }
            (&Ty::Bound(a), &Ty::Bound(b)) => {
                if a != b {
                    CompileError::new(
                        sp,
                        format!("mismatched bound types, expected `{}`, found `{}`", a, b),
                    )
                } else {
                    Ok(Ty::Bound(a))
                }
            }
            (Ty::Fn(a_arg, a_ret), Ty::Fn(b_arg, b_ret)) => {
                let arg = self.eq_tys(sp, a_arg.as_ref().clone(), b_arg.as_ref().clone())?;
                let ret = self.eq_tys(sp, a_ret.as_ref().clone(), b_ret.as_ref().clone())?;
                Ok(Ty::Fn(Box::new(arg), Box::new(ret)))
            }
            _ => CompileError::new(
                sp,
                format!("mismatched types, expected `{}`, found `{}`", a, b),
            ),
        }
    }

    fn check_def(&mut self, def: &Definition<'a>) -> Result<(), CompileError> {
        let def_ty = self.ast_ty_to_ty(&def.ty)?;
        self.types.insert(def.id, def_ty.clone());
        let body_ty = self.check_body(&def.body)?;
        self.eq_tys(def.head_span, def_ty, body_ty)?;
        Ok(())
    }

    fn check_body(&mut self, body: &Body<'a>) -> Result<Ty, CompileError> {
        self.check_expr(&body.expr)
    }

    fn check_expr(&mut self, e: &Expr<'a>) -> Result<Ty, CompileError> {
        let ty = match e.kind {
            ExprKind::Lambda { ref arg, ref expr } => {
                let arg_infer = self.infer_ty();
                self.types.insert(arg.id, arg_infer.clone());

                let expr_ty = self.check_expr(expr)?;
                Ty::Fn(Box::new(arg_infer), Box::new(expr_ty))
            }
            ExprKind::App { ref func, ref arg } => {
                let func_ty = self.check_expr(func)?;
                let arg_ty = self.check_expr(arg)?;

                let ret = self.infer_ty();
                let expected_func = Ty::Fn(Box::new(arg_ty), Box::new(ret.clone()));
                self.eq_tys(e.span, func_ty, expected_func)?;
                ret
            }
            ExprKind::Path(ref path) => match self.res.get(&path.id).unwrap() {
                Res::Def(_) => todo!(),
                Res::BoundVar(_) => unreachable!("ty as expr"),
                Res::LambdaArg(arg) => self.types.get(arg).unwrap().clone(),
            },
        };

        self.types.insert(e.id, ty.clone());
        Ok(ty.clone())
    }
}

pub fn typeck<'a>(
    module: &Module<'a>,
    res: &HashMap<AstId, Res>,
) -> Result<HashMap<AstId, Ty>, CompileError> {
    let mut types = HashMap::new();
    for def in &module.definitions {
        let bound_var_map = def
            .variables
            .iter()
            .zip(0..)
            .map(|(var, idx)| (var.id, idx))
            .collect();

        let mut ctx = TypeckCtxt {
            module,
            res,
            bound_var_map,
            inference_vars: TVec::new(),
            types: HashMap::new(),
        };
        ctx.check_def(&def.item)?;

        for (id, ty) in ctx.types.clone() {
            types.insert(id, ctx.deep_resolve(ty.clone()));
        }
    }

    Ok(types)
}
