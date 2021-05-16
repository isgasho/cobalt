use co_diag::Span;

pub mod resolve;
pub mod typeck;
pub mod visit;

#[derive(Debug, Hash, Copy, Clone, PartialEq, Eq)]
pub struct AstId(pub usize);

#[derive(Debug, Clone, Copy)]
pub struct BoundVar<'a> {
    pub id: AstId,
    pub span: Span<'a>,
    pub ident: &'a str,
}

#[derive(Debug)]
pub struct Binder<'a, T> {
    pub variables: Vec<BoundVar<'a>>,
    pub item: T,
}

#[derive(Debug)]
pub struct Module<'a> {
    pub definitions: Vec<Binder<'a, Definition<'a>>>,
}

#[derive(Debug)]
pub struct Definition<'a> {
    pub id: AstId,
    pub head_span: Span<'a>,
    pub ident: &'a str,
    pub ty: Type<'a>,
    pub body: Body<'a>,
}

#[derive(Debug)]
pub enum Type<'a> {
    Func(Box<Type<'a>>, Box<Type<'a>>),
    Path(Path<'a>),
}

#[derive(Debug)]
pub struct Body<'a> {
    pub expr: Expr<'a>,
}

#[derive(Debug, Clone, Copy)]
pub struct LambdaArg<'a> {
    pub id: AstId,
    pub ident: &'a str,
}

#[derive(Debug)]
pub struct Expr<'a> {
    pub id: AstId,
    pub span: Span<'a>,
    pub kind: ExprKind<'a>,
}

#[derive(Debug)]
pub enum ExprKind<'a> {
    Lambda {
        arg: LambdaArg<'a>,
        expr: Box<Expr<'a>>,
    },
    App {
        func: Box<Expr<'a>>,
        arg: Box<Expr<'a>>,
    },
    Path(Path<'a>),
}

#[derive(Debug)]
pub struct Path<'a> {
    pub id: AstId,
    pub ident: &'a str,
    pub span: Span<'a>,
}
