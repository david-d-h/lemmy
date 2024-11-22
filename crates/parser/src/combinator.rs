use ast::untyped::Expression;

use crate::spanned::{Span, Spanned};
use crate::token::{Token, TokenKind};
use crate::{Parser, Result};

#[inline]
pub fn any_expression(parser: &mut Parser<'_>) -> Result<Expression> {
    crate::expression::expression(parser, None, 0)
}

#[inline]
pub fn just<'src>(kind: TokenKind) -> impl Fn(&mut Parser<'src>) -> Result<Token> {
    move |parser| parser.expect(kind).map(Spanned::into_inner)
}

#[inline]
pub fn map<'src, T, U>(
    inner: impl Fn(&mut Parser<'src>) -> T,
    f: fn(T) -> U,
) -> impl Fn(&mut Parser<'src>) -> U {
    move |parser| f(inner(parser))
}

#[inline]
pub fn try_map<'src, T, U>(
    inner: impl Fn(&mut Parser<'src>) -> Result<T>,
    f: fn(T) -> U,
) -> impl Fn(&mut Parser<'src>) -> Result<U> {
    move |parser| inner(parser).map(f)
}

pub fn with_checkpoint<'src, T>(
    inner: impl Fn(&mut Parser<'src>) -> Result<T>,
) -> impl Fn(&mut Parser<'src>) -> Result<T> {
    move |parser| {
        let backup = parser.lexer.cursor();
        inner(parser).inspect_err(move |_| parser.lexer.insert_cursor_state(backup))
    }
}

#[inline]
pub fn delimited<'src, T>(
    left: TokenKind,
    inner: impl Fn(&mut Parser<'src>) -> Result<T>,
    right: TokenKind,
) -> impl Fn(&mut Parser<'src>) -> Result<T> {
    prefixed([left], terminated(inner, right))
}

pub fn prefixed<'src, const N: usize, T>(
    by: [TokenKind; N],
    inner: impl Fn(&mut Parser<'src>) -> Result<T>,
) -> impl Fn(&mut Parser<'src>) -> Result<T> {
    move |parser| {
        by.try_map(|kind| parser.expect(kind))
            .and_then(|_| inner(parser))
    }
}

pub fn terminated<'src, T>(
    inner: impl Fn(&mut Parser<'src>) -> Result<T>,
    by: TokenKind,
) -> impl Fn(&mut Parser<'src>) -> Result<T> {
    move |parser| inner(parser).and_then(move |out| parser.expect(by).map(|_| out))
}

pub fn maybe<'src, T>(
    inner: impl Fn(&mut Parser<'src>) -> Result<T>,
) -> impl Fn(&mut Parser<'src>) -> Option<T> {
    map(with_checkpoint(inner), Result::ok)
}

pub fn many<'src, T>(
    generator: impl Fn(&mut Parser<'src>) -> Option<T>,
) -> impl Fn(&mut Parser<'src>) -> Result<Vec<T>> {
    move |parser| {
        let mut items = Vec::new();
        while let Some(item) = generator(parser) {
            items.push(item);
        }
        Ok(items)
    }
}

pub fn separated<'src, T>(
    generator: impl Fn(&mut Parser<'src>) -> Result<T>,
    by: TokenKind,
) -> impl Fn(&mut Parser<'src>) -> Result<Vec<T>> {
    move |parser| {
        let mut items = Vec::new();
        while let Some(item) = maybe(&generator)(parser) {
            items.push(item);
            if !parser.eat(by) {
                break;
            }
        }
        if items.len() != 0 {
            _ = maybe(just(by))(parser);
        }
        Ok(items)
    }
}

pub fn spanned<'src, T>(
    inner: impl Fn(&mut Parser<'src>) -> T,
) -> impl Fn(&mut Parser<'src>) -> Spanned<T> {
    move |parser| {
        let start = parser.lexer.cursor().anchor;
        let item = inner(parser);
        let end = parser.lexer.cursor().current;
        Spanned {
            span: Span { start, end },
            item,
        }
    }
}
