use crate::combinator::*;
use crate::token::{Token, TokenKind};
use crate::{Parser, Result};

#[inline]
pub fn arg(parser: &mut Parser<'_>) -> Result<ast::Arg> {
    match just(TokenKind::Identifier)(parser)? {
        Token::Identifier(symbol) => Ok(ast::Arg { symbol }),
        _ => unreachable!(),
    }
}

#[inline]
pub fn args(parser: &mut Parser<'_>) -> Result<Vec<ast::Arg>> {
    delimited(
        TokenKind::LParen,
        separated(arg, TokenKind::Comma),
        TokenKind::RParen,
    )(parser)
}
