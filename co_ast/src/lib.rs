use co_diag::Span;

#[derive(Debug)]
pub struct Module<'a> {
    pub definitions: Vec<Definition<'a>>,
}

#[derive(Debug)]
pub struct Definition<'a> {
    pub head_span: Span<'a>,
    pub ident: &'a str,
    pub ty: Type<'a>,
    pub body: Body<'a>,
}

#[derive(Debug)]
pub enum Type<'a> {
    Func(Box<Type<'a>>, Box<Type<'a>>),
    Path(&'a str),
}

#[derive(Debug)]
pub struct Body<'a> {
    pub expr: Expr<'a>,
}

#[derive(Debug)]
pub enum Expr<'a> {
    Lambda { arg: &'a str, expr: Box<Expr<'a>> },
    App { func: Box<Expr<'a>>, arg: Box<Expr<'a>> },
    Path(&'a str),
}
