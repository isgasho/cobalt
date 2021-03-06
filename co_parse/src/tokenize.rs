use co_ast as ast;
use co_diag::{CompileError, Span};

use logos::Logos;

use std::iter::Peekable;

#[derive(Logos, Debug, PartialEq, Clone, Copy)]
#[logos(extras = u32)]
pub(crate) enum Token<'a> {
    #[token("::")]
    DoubleColon,
    #[token("->")]
    Arrow,
    #[token("\\")]
    Lambda,
    #[token(";")]
    SemiColon,
    #[token(",")]
    Comma,
    #[token("=")]
    Eq,
    #[token("[")]
    LBracket,
    #[token("(")]
    LBrace,
    #[token("]")]
    RBracket,
    #[token(")")]
    RBrace,
    #[token("for")]
    For,
    #[regex("[a-zA-Z_']+", |lex| lex.slice())]
    Ident(&'a str),
    EOF,
    #[error]
    #[token("\n", |lex| {
        lex.extras += 1;
        logos::Skip
    })]
    #[regex(r"[ \t\n\f]+", logos::skip)]
    Error,
}

struct Tokenizer<'a> {
    file: &'a str,
    lex: logos::Lexer<'a, Token<'a>>,
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = (Token<'a>, Span<'a>);

    fn next(&mut self) -> Option<Self::Item> {
        Some(
            self.lex
                .next()
                .map(|tok| {
                    (
                        tok,
                        Span::new(
                            self.lex.source(),
                            self.file,
                            self.lex.extras,
                            self.lex.span(),
                        ),
                    )
                })
                .unwrap_or_else(|| {
                    let sp = self.lex.span();
                    (
                        Token::EOF,
                        Span::new(
                            self.lex.source(),
                            self.file,
                            self.lex.extras,
                            sp.end..sp.end,
                        ),
                    )
                }),
        )
    }
}

pub(crate) struct Lexer<'a>(Peekable<Tokenizer<'a>>, usize);

impl<'a> Lexer<'a> {
    pub(crate) fn new(file: &'a str, src: &'a str) -> Lexer<'a> {
        Lexer(
            Tokenizer {
                file,
                lex: Token::lexer(src),
            }
            .peekable(),
            0,
        )
    }

    pub(crate) fn advance(&mut self) -> (Token<'a>, Span<'a>) {
        self.0.next().unwrap()
    }

    pub(crate) fn next_id(&mut self) -> ast::AstId {
        let id = self.1;
        self.1 += 1;
        ast::AstId(id)
    }

    pub(crate) fn eat(&mut self, expected: Token<'a>) -> Result<(), CompileError> {
        let (tok, sp) = self.advance();
        if tok != expected {
            CompileError::expected(&expected, sp)
        } else {
            Ok(())
        }
    }

    pub(crate) fn try_eat(&mut self, expected: Token<'a>) -> bool {
        let (tok, _) = self.peek();
        if tok == expected {
            self.advance();
            true
        } else {
            false
        }
    }

    pub(crate) fn expect_ident(&mut self) -> Result<(&'a str, Span<'a>), CompileError> {
        let (tok, sp) = self.advance();
        match tok {
            Token::Ident(s) => Ok((s, sp)),
            _ => CompileError::expected(&"identifier", sp),
        }
    }

    pub(crate) fn opt_ident(&mut self) -> Option<(&'a str, Span<'a>)> {
        let (tok, _) = self.peek();
        match tok {
            Token::Ident(_) => Some(self.expect_ident().unwrap()),
            _ => None,
        }
    }

    pub(crate) fn peek(&mut self) -> (Token<'a>, Span<'a>) {
        *self.0.peek().unwrap()
    }
}
