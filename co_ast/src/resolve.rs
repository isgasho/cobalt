use super::*;

use visit::{Visitor, Walkable};

use co_diag::CompileError;

use std::collections::HashMap;

#[derive(Debug, Clone, Copy)]
pub enum Res {
    Def(AstId),
    BoundVar(AstId),
    LambdaArg(AstId),
}

struct ResolveVisitor<'a> {
    resolutions: HashMap<AstId, Res>,
    defs: HashMap<&'a str, AstId>,
    bound_vars: Vec<BoundVar<'a>>,
    lambda_args: Vec<LambdaArg<'a>>,
}

impl<'a> Visitor<'a> for ResolveVisitor<'a> {
    fn visit_binder<T: Walkable<'a>>(&mut self, b: &Binder<'a, T>) -> Result<(), CompileError> {
        let old_len = self.bound_vars.len();
        for &var in &b.variables {
            if let Some(v) = self.bound_vars.iter().find(|v| v.ident == var.ident) {
                CompileError::build(
                    var.span,
                    format!(
                        "the bound variable `{}` shadows another variable already in scope",
                        v.ident
                    ),
                )
                .with_help("first declared here")
                .with_location(v.span)
                .build()?;
            }

            self.bound_vars.push(var);
        }
        b.item.visit_with(self)?;
        self.bound_vars.truncate(old_len);
        Ok(())
    }

    fn visit_expr(&mut self, e: &Expr<'a>) -> Result<(), CompileError> {
        match e.kind {
            ExprKind::Lambda { arg, ref expr } => {
                self.lambda_args.push(arg);
                expr.visit_with(self)?;
                self.lambda_args.pop();
                Ok(())
            }
            _ => e.walk_with(self),
        }
    }

    fn visit_path(&mut self, p: &Path<'a>) -> Result<(), CompileError> {
        let res = (|| {
            for arg in &self.lambda_args {
                if arg.ident == p.ident {
                    return Ok(Res::LambdaArg(arg.id));
                }
            }

            for var in &self.bound_vars {
                if var.ident == p.ident {
                    return Ok(Res::BoundVar(var.id));
                }
            }

            if let Some(&def) = self.defs.get(p.ident) {
                return Ok(Res::Def(def));
            }

            CompileError::new(
                p.span,
                format_args!("Cannot find identifier `{}` in this scope", p.ident),
            )
        })()?;

        self.resolutions.insert(p.id, res);

        Ok(())
    }
}

pub fn resolve_mod<'a>(module: &Module<'a>) -> Result<HashMap<AstId, Res>, CompileError> {
    let mut visitor = ResolveVisitor {
        resolutions: HashMap::new(),
        defs: module
            .definitions
            .iter()
            .map(|def| (def.item.ident, def.item.id))
            .collect(),
        bound_vars: Vec::new(),
        lambda_args: Vec::new(),
    };

    visit::Visitor::visit_mod(&mut visitor, module)?;

    Ok(visitor.resolutions)
}
