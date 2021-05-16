use super::*;
use co_diag::CompileError;
use resolve::Res;
use std::collections::HashMap;
use tindex::{TIndex, TVec};
use visit::{Visitor, Walkable};

#[derive(Debug)]
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

#[derive(Debug)]
pub enum Ty {
    Infer(InferTy),
    Bound(usize),
    Fn(Box<Ty>, Box<Ty>),
}

enum Infer {
    Known(Ty),
    Eq(InferTy),
    Unknown,
}

struct TypeckCtxt<'a, 'b> {
    module: &'a Module<'a>,
    bound_var_map: HashMap<AstId, usize>,
    res: &'a HashMap<AstId, Res>,
    inference_vars: TVec<InferTy, Infer>,
    types: &'b mut HashMap<AstId, Ty>,
}

impl<'a> TypeckCtxt<'a, '_> {
    fn ast_ty_to_ty(&mut self, ast_ty: &Type) -> Ty {
        todo!()
    }

    fn infer_ty(&mut self) -> Ty {
        Ty::Infer(self.inference_vars.push(Infer::Unknown))
    }
}

impl<'a> Visitor<'a> for TypeckCtxt<'a, '_> {
    fn visit_binder<T: Walkable<'a>>(&mut self, b: &Binder<'a, T>) -> Result<(), CompileError> {
        unreachable!("visit_binder")
    }

    fn visit_ty(&mut self, t: &Type<'a>) -> Result<(), CompileError> {
        unreachable!("visit_ty")
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
            types: &mut types,
        };
        def.item.visit_with(&mut ctx)?;
    }

    Ok(types)
}
