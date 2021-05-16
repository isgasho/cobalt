use co_diag::CompileError;

pub fn compile<'a>(file: &'a str, src: &'a str) -> Result<co_ast::Module<'a>, CompileError> {
    let ast = co_parse::parse_mod(file, src)?;
    Ok(ast)
}
