use co_diag::CompileError;

pub fn compile<'a>(file: &'a str, src: &'a str) -> Result<co_ast::Module<'a>, CompileError> {
    let ast = co_parse::parse_mod(file, src)?;
    let res = co_ast::resolve::resolve_mod(&ast)?;
    let types = co_ast::typeck::typeck(&ast, &res)?;
    dbg!(types);
    Ok(ast)
}
