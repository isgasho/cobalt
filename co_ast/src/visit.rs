use super::*;

use co_diag::CompileError;

pub trait Visitor<'a>: Sized {
    fn visit_mod(&mut self, m: &Module<'a>) -> Result<(), CompileError> {
        m.walk_with(self)
    }

    fn visit_binder<T: Walkable<'a>>(&mut self, b: &Binder<'a, T>) -> Result<(), CompileError> {
        b.walk_with(self)
    }

    fn visit_def(&mut self, d: &Definition<'a>) -> Result<(), CompileError> {
        d.walk_with(self)
    }

    fn visit_ty(&mut self, t: &Type<'a>) -> Result<(), CompileError> {
        t.walk_with(self)
    }

    fn visit_body(&mut self, b: &Body<'a>) -> Result<(), CompileError> {
        b.walk_with(self)
    }

    fn visit_expr(&mut self, e: &Expr<'a>) -> Result<(), CompileError> {
        e.walk_with(self)
    }

    fn visit_path(&mut self, p: &Path<'a>) -> Result<(), CompileError> {
        p.walk_with(self)
    }
}

pub trait Walkable<'a> {
    fn visit_with(&self, v: &mut impl Visitor<'a>) -> Result<(), CompileError>;

    fn walk_with(&self, v: &mut impl Visitor<'a>) -> Result<(), CompileError>;
}

impl<'a> Walkable<'a> for Module<'a> {
    fn visit_with(&self, v: &mut impl Visitor<'a>) -> Result<(), CompileError> {
        v.visit_mod(self)
    }

    fn walk_with(&self, v: &mut impl Visitor<'a>) -> Result<(), CompileError> {
        for d in self.definitions.iter() {
            d.visit_with(v)?
        }

        Ok(())
    }
}

impl<'a, T: Walkable<'a>> Walkable<'a> for Binder<'a, T> {
    fn visit_with(&self, v: &mut impl Visitor<'a>) -> Result<(), CompileError> {
        v.visit_binder(self)
    }

    fn walk_with(&self, v: &mut impl Visitor<'a>) -> Result<(), CompileError> {
        let &Binder {
            variables: _,
            ref item,
        } = self;

        item.visit_with(v)
    }
}

impl<'a> Walkable<'a> for Definition<'a> {
    fn visit_with(&self, v: &mut impl Visitor<'a>) -> Result<(), CompileError> {
        v.visit_def(self)
    }

    fn walk_with(&self, v: &mut impl Visitor<'a>) -> Result<(), CompileError> {
        let &Definition {
            head_span: _,
            id: _,
            ident: _,
            ref ty,
            ref body,
        } = self;

        ty.visit_with(v)?;
        body.visit_with(v)
    }
}

impl<'a> Walkable<'a> for Type<'a> {
    fn visit_with(&self, v: &mut impl Visitor<'a>) -> Result<(), CompileError> {
        v.visit_ty(self)
    }

    fn walk_with(&self, v: &mut impl Visitor<'a>) -> Result<(), CompileError> {
        match self {
            Type::Func(input, output) => {
                input.visit_with(v)?;
                output.visit_with(v)
            }
            Type::Path(path) => path.visit_with(v),
        }
    }
}

impl<'a> Walkable<'a> for Body<'a> {
    fn visit_with(&self, v: &mut impl Visitor<'a>) -> Result<(), CompileError> {
        v.visit_body(self)
    }

    fn walk_with(&self, v: &mut impl Visitor<'a>) -> Result<(), CompileError> {
        let &Body { ref expr } = self;

        expr.visit_with(v)
    }
}

impl<'a> Walkable<'a> for Expr<'a> {
    fn visit_with(&self, v: &mut impl Visitor<'a>) -> Result<(), CompileError> {
        v.visit_expr(self)
    }

    fn walk_with(&self, v: &mut impl Visitor<'a>) -> Result<(), CompileError> {
        let &Expr { id: _, ref kind } = self;

        match kind {
            ExprKind::Lambda { arg: _, expr } => expr.visit_with(v),
            ExprKind::App { func, arg } => {
                func.visit_with(v)?;
                arg.visit_with(v)
            }
            ExprKind::Path(p) => p.visit_with(v),
        }
    }
}

impl<'a> Walkable<'a> for Path<'a> {
    fn visit_with(&self, v: &mut impl Visitor<'a>) -> Result<(), CompileError> {
        v.visit_path(self)
    }

    fn walk_with(&self, _v: &mut impl Visitor<'a>) -> Result<(), CompileError> {
        Ok(())
    }
}
