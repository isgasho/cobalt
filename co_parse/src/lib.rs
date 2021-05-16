use co_ast as ast;
use co_diag::{CompileError, Span};

mod tokenize;
use tokenize::{Lexer, Token};

pub fn parse_mod<'a>(file: &'a str, src: &'a str) -> Result<ast::Module<'a>, CompileError> {
    let lex = &mut Lexer::new(file, src);
    let mut definitions = Vec::new();
    loop {
        if lex.try_eat(Token::EOF) {
            break;
        } else {
            definitions.push(parse_definition(lex)?);
        }
    }

    Ok(ast::Module {
        definitions
    })
}

fn parse_definition<'a>(lex: &mut Lexer<'a>) -> Result<ast::Definition<'a>, CompileError> {
    let (ident, head_span) = lex.expect_ident()?;

    lex.eat(Token::DoubleColon)?;

    let ty = parse_ty(lex, true)?;

    lex.eat(Token::Eq)?;

    let body = parse_body(lex)?;

    lex.eat(Token::SemiColon)?;

    Ok(ast::Definition {
        head_span,
        ident,
        ty,
        body,
    })
}

fn parse_ty<'a>(lex: &mut Lexer<'a>, with_arrow: bool) -> Result<ast::Type<'a>, CompileError> {
    if with_arrow {
        let arg = parse_ty(lex, false)?;
        if lex.try_eat(Token::Arrow) {
            let ret = parse_ty(lex, true)?;
            Ok(ast::Type::Func(Box::new(arg), Box::new(ret)))
        } else {
            Ok(arg)
        }
    } else {
        Ok(ast::Type::Path(parse_path(lex)?))
    }
}

fn parse_body<'a>(lex: &mut Lexer<'a>) -> Result<ast::Body<'a>, CompileError> {
    let expr = parse_expr(lex)?;

    Ok(ast::Body {
        expr,
    })
}

fn parse_expr<'a>(lex: &mut Lexer<'a>) -> Result<ast::Expr<'a>, CompileError> {
    let expr = match lex.peek() {
        (Token::Lambda, _sp) => {
            lex.eat(Token::Lambda).unwrap();
            let (arg, _) = lex.expect_ident()?;
            lex.eat(Token::Arrow)?;
            let expr = Box::new(parse_expr(lex)?);
            ast::Expr::Lambda {
                arg,
                expr,
            }
        }
        (Token::Ident(_), _) => {
            let path = parse_path(lex)?;
            ast::Expr::Path(path)
        }
        (_, sp) => CompileError::expected(&"a lambda or a path", sp)?,
    };

    match lex.peek() {
        (Token::Lambda | Token::Ident(_), _) => {
            let arg = Box::new(parse_expr(lex)?);
            Ok(ast::Expr::App { func: Box::new(expr), arg })
        }
        _ => Ok(expr)
    }
}

fn parse_path<'a>(lex: &mut Lexer<'a>) -> Result<&'a str, CompileError> {
    lex.expect_ident().map(|(id, _)| id)
}
