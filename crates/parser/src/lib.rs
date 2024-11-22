#![feature(
    generic_const_exprs,
    array_try_from_fn,
    if_let_guard,
    let_chains,
    array_try_map,
    trace_macros,
    trait_alias,
    decl_macro
)]
#![allow(incomplete_features)]

use std::array;

pub mod combinator;
pub mod common;
pub mod error;
pub mod expression;
pub mod lexer;
pub mod spanned;
pub mod statement;
pub mod token;

use error::ParseError;
use lexer::Lexer;
use spanned::Spanned;
use token::{Token, TokenKind};

pub use expression::expression;

pub type Result<T, E = ParseError> = std::result::Result<T, E>;

#[derive(Debug, Clone)]
pub struct Parser<'src> {
    lexer: Lexer<'src>,
}

impl<'src> Parser<'src> {
    pub fn new(source: &'src str) -> Parser<'src> {
        Parser {
            lexer: Lexer::new(source),
        }
    }

    #[inline]
    pub fn advance(&mut self) -> Result<Spanned<Token>> {
        self.lexer
            .next()
            .ok_or_else(|| ParseError::out_of_tokens(self.lexer.source_len()))?
            .map_err(ParseError::LexError)
    }

    #[inline]
    pub fn peek_kind(&mut self) -> Option<TokenKind> {
        self.try_peek_kind().ok()
    }

    #[inline]
    pub fn try_peek_kind(&mut self) -> Result<TokenKind> {
        self.peek().map(|it| it.item.kind())
    }

    #[inline]
    pub fn peek(&mut self) -> Result<Spanned<Token>> {
        self.peek_by::<1>()
    }

    #[inline]
    pub fn peek_by<const N: usize>(&mut self) -> Result<Spanned<Token>>
    where
        [(); { N > 0 } as usize]:,
    {
        Ok(self.peek_n::<N>()?[N - 1].clone())
    }

    pub fn peek_n<const N: usize>(&mut self) -> Result<[Spanned<Token>; N]>
    where
        [(); { N > 0 } as usize]:,
    {
        let state = self.lexer.cursor();

        let items = array::try_from_fn(|_| self.advance());

        self.lexer.insert_cursor_state(state);

        items
    }

    pub fn expect(&mut self, kind: TokenKind) -> Result<Spanned<Token>> {
        match self.advance()? {
            token if token.item.kind() == kind => Ok(token),
            token => Err(ParseError::unexpected_token([kind], token.item, token.span)),
        }
    }

    #[inline]
    pub fn eat(&mut self, kind: TokenKind) -> bool {
        if self.peek_kind() == Some(kind) {
            self.advance().expect("just peeked token");
            true
        } else {
            false
        }
    }

    pub fn one_of<const N: usize>(&mut self, allowed: [TokenKind; N]) -> Result<Spanned<Token>>
    where
        [(); { N > 0 } as usize]:,
    {
        match self.advance()? {
            token if allowed.contains(&token.item.kind()) => Ok(token),
            token => Err(ParseError::unexpected_token(
                allowed, token.item, token.span,
            ))?,
        }
    }

    pub fn peek_one_of<const N: usize>(&mut self, allowed: [TokenKind; N]) -> Result<Spanned<Token>>
    where
        [(); { N > 0 } as usize]:,
    {
        let checkpoint = self.lexer.cursor();
        let result = self.one_of(allowed);
        self.lexer.insert_cursor_state(checkpoint);
        result
    }
}

#[cfg(test)]
macro_rules! insta_tests {
    ($($([$($mod:tt)*])? $name:ident as $fn:ident { $($code:tt)* })*) => ($($crate::insta_tests!(@single $([$($mod)*])? $name $fn [$($code)*]);)*);
    (@single $name:ident $fn:ident [$($code:tt)*]) => {
        #[test]
        #[cfg(test)]
        fn $name() {
            ::insta::assert_debug_snapshot!($fn(::core::stringify!($($code)*)))
        }
    };
    (@single [compact] $name:ident $fn:ident [$($code:tt)*]) => {
        #[test]
        #[cfg(test)]
        fn $name() {
            ::insta::assert_compact_debug_snapshot!($fn(::core::stringify!($($code)*)))
        }
    };
}

#[cfg(test)]
pub(crate) use insta_tests;
