use thiserror::Error;

use crate::lexer::LexError;
use crate::spanned::Span;
use crate::token::{Token, TokenKind};

mod util {
    use core::fmt;

    #[inline]
    pub fn slice_items<D: fmt::Display>(items: &[D]) -> String {
        items
            .into_iter()
            .map(ToString::to_string)
            .collect::<Vec<_>>()
            .join(", ")
    }
}

#[derive(Debug, Error)]
#[error("expected more tokens, but lexer has already yielded <EOF>")]
#[readonly::make]
pub struct OutOfTokensError {
    end: usize,
}

#[derive(Debug, Error)]
#[error("expected \"{expected}\", found \"{actual}\"")]
#[readonly::make]
pub struct UnexpectedTokenError {
    expected: TokenKind,
    actual: Token,
    span: Span,
}

#[derive(Debug, Error)]
#[error(
    "unexpected token {actual:?}, expected one of {}",
    util::slice_items(&allowed)
)]
#[readonly::make]
pub struct ExpectedOneOfError {
    allowed: Vec<TokenKind>,
    actual: Token,
    span: Span,
}

#[derive(Debug, Error)]
#[error(transparent)]
pub enum ParseError {
    LexError(#[from] LexError),
    OutOfTokensError(#[from] OutOfTokensError),
    UnexpectedTokenError(#[from] UnexpectedTokenError),
    ExpectedOneOfError(#[from] ExpectedOneOfError),
}

impl ParseError {
    pub fn out_of_tokens(end: usize) -> Self {
        Self::OutOfTokensError(OutOfTokensError { end })
    }

    pub fn unexpected_token<const N: usize>(
        allowed: [TokenKind; N],
        actual: Token,
        span: Span,
    ) -> Self
    where
        [(); { N > 0 } as usize]:,
    {
        if N == 1 {
            Self::UnexpectedTokenError(UnexpectedTokenError {
                expected: allowed[0],
                actual,
                span,
            })
        } else {
            Self::expected_one_of(allowed, actual, span)
        }
    }

    pub fn expected_one_of<const N: usize>(
        allowed: [TokenKind; N],
        actual: Token,
        span: Span,
    ) -> Self
    where
        [(); { N > 0 } as usize]:,
    {
        Self::ExpectedOneOfError(ExpectedOneOfError {
            allowed: allowed.to_vec(),
            actual,
            span,
        })
    }
}
