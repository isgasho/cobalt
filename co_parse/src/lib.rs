use co_ast as ast;
use co_diag::CompileError;

mod tokenize;
use tokenize::{Lexer, Token};

pub fn parse_mod<'a>(file: &'a str, src: &'a str) -> Result<ast::Module<'a>, CompileError> {
    let lex = &mut Lexer::new(file, src);
    let mut definitions = Vec::new();
    loop {
        if lex.try_eat(Token::EOF) {
            break;
        } else {
            definitions.push(parse_opt_binder(lex, |lex| parse_definition(lex))?);
        }
    }

    Ok(ast::Module { definitions })
}

fn parse_opt_binder<'a, T, F>(lex: &mut Lexer<'a>, f: F) -> Result<ast::Binder<'a, T>, CompileError>
where
    F: FnOnce(&mut Lexer<'a>) -> Result<T, CompileError>,
{
    if lex.try_eat(Token::For) {
        lex.eat(Token::LBracket)?;
        let mut variables = Vec::new();
        loop {
            if let Some((ident, span)) = lex.opt_ident() {
                variables.push(ast::BoundVar {
                    id: lex.next_id(),
                    span,
                    ident,
                });
            } else {
                break;
            }

            if !lex.try_eat(Token::Comma) {
                break;
            }
        }

        lex.eat(Token::RBracket)?;

        Ok(ast::Binder {
            variables,
            item: f(lex)?,
        })
    } else {
        f(lex).map(|item| ast::Binder {
            variables: Vec::new(),
            item,
        })
    }
}

fn parse_definition<'a>(lex: &mut Lexer<'a>) -> Result<ast::Definition<'a>, CompileError> {
    let (ident, head_span) = lex.expect_ident()?;

    lex.eat(Token::DoubleColon)?;

    let ty = parse_ty(lex, true)?;

    lex.eat(Token::Eq)?;

    let body = parse_body(lex)?;

    lex.eat(Token::SemiColon)?;

    Ok(ast::Definition {
        id: lex.next_id(),
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
    } else if lex.try_eat(Token::LBrace) {
        let ty = parse_ty(lex, true)?;
        lex.eat(Token::RBrace)?;
        Ok(ty)
    } else {
        Ok(ast::Type::Path(parse_path(lex)?))
    }
}

fn parse_body<'a>(lex: &mut Lexer<'a>) -> Result<ast::Body<'a>, CompileError> {
    let expr = parse_expr(lex, None)?;

    Ok(ast::Body { expr })
}

fn parse_expr<'a>(
    lex: &mut Lexer<'a>,
    app: Option<ast::Expr<'a>>,
) -> Result<ast::Expr<'a>, CompileError> {
    let (expr_kind, span) = match lex.peek() {
        (Token::Lambda, sp) => {
            lex.eat(Token::Lambda).unwrap();
            let (ident, _span) = lex.expect_ident()?;
            let arg = ast::LambdaArg {
                id: lex.next_id(),
                ident,
            };
            lex.eat(Token::Arrow)?;
            let expr = Box::new(parse_expr(lex, None)?);
            (ast::ExprKind::Lambda { arg, expr }, sp)
        }
        (Token::Ident(_), sp) => {
            let path = parse_path(lex)?;
            (ast::ExprKind::Path(path), sp)
        }
        (Token::LBrace, sp) => {
            lex.eat(Token::LBrace).unwrap();
            let expr = parse_expr(lex, None)?;
            lex.eat(Token::RBrace)?;
            (expr.kind, sp)
        }
        (_, sp) => CompileError::expected(&"a lambda or a path", sp)?,
    };

    let expr = ast::Expr {
        id: lex.next_id(),
        span,
        kind: expr_kind,
    };

    let expr = if let Some(app) = app {
        ast::Expr {
            id: lex.next_id(),
            span,
            kind: ast::ExprKind::App {
                func: Box::new(app),
                arg: Box::new(expr),
            },
        }
    } else {
        expr
    };

    match lex.peek() {
        (Token::Lambda | Token::Ident(_) | Token::LBrace, span) => parse_expr(lex, Some(expr)),
        _ => Ok(expr),
    }
}

fn parse_path<'a>(lex: &mut Lexer<'a>) -> Result<ast::Path<'a>, CompileError> {
    lex.expect_ident().map(|(ident, span)| ast::Path {
        id: lex.next_id(),
        ident,
        span,
    })
}
