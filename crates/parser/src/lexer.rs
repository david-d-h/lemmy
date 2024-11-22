use ecow::EcoString;
use num_bigint::BigInt;
use thiserror::Error;

use crate::token::{Token, TokenKind};

use crate::spanned::{Span, Spanned};

#[derive(Debug, Error)]
#[error("unterminated string literal found")]
#[readonly::make]
pub struct StringTerminationError {
    span: Span,
}

#[derive(Debug, Error)]
#[error("invalid token \"{token}\" found")]
#[readonly::make]
pub struct SingleTokenError {
    pub token: char,
    span: Span,
}

#[derive(Debug, Error)]
#[error(transparent)]
pub enum LexError {
    StringTerminationError(#[from] StringTerminationError),
    SingleTokenError(#[from] SingleTokenError),
}

#[derive(Debug, Copy, Clone)]
pub struct Lexer<'src> {
    bytes: &'src [u8],
    length: usize,
    cursor: Cursor,
    last: Option<TokenKind>,
}

#[derive(Debug, Default, Copy, Clone, Hash, Eq, PartialEq)]
pub struct Cursor {
    pub(crate) anchor: usize,
    pub(crate) current: usize,
}

impl<'src> Lexer<'src> {
    pub fn new<S: AsRef<str> + ?Sized>(source: &'src S) -> Self {
        let bytes = source.as_ref().as_bytes();
        Self {
            bytes,
            length: bytes.len(),
            cursor: Cursor::default(),
            last: None,
        }
    }

    #[inline]
    pub fn insert_cursor_state(&mut self, state: Cursor) {
        self.cursor = state;
    }

    #[inline(always)]
    pub fn source(&self) -> &'src str {
        // yes this is not safe
        unsafe { core::str::from_utf8_unchecked(&self.bytes) }
    }

    #[inline]
    pub fn source_len(&self) -> usize {
        self.length
    }

    #[inline]
    pub const fn cursor(&self) -> Cursor {
        self.cursor
    }

    #[inline]
    pub fn rest(&self) -> &[u8] {
        &self.bytes[self.cursor.current..]
    }

    #[inline]
    fn peek(&self) -> Option<u8> {
        self.rest().get(0).cloned()
    }

    #[inline]
    fn peek_by(&self, by: usize) -> Option<u8> {
        self.rest().get(by).cloned()
    }

    fn peek_n<const N: usize>(&mut self) -> Option<[u8; N]> {
        self.rest().get(..N).map(|it| {
            it.try_into()
                .expect("the slice can only contain `N` elements")
        })
    }

    #[inline]
    fn consume<F>(&mut self, mut predicate: F)
    where
        F: FnMut(u8) -> bool,
    {
        let mut end = 0;

        while match self.peek_by(end) {
            Some(byte) if predicate(byte) => true,
            _ => false,
        } {
            end += 1;
        }

        self.cursor.increment(end)
    }

    #[inline]
    fn spanned_bytes(&self) -> &'src [u8] {
        &self.bytes[self.cursor.anchor..self.cursor.current]
    }

    #[inline]
    fn spanned_symbol(&self) -> &'src str {
        unsafe { core::str::from_utf8_unchecked(self.spanned_bytes()) }
    }
}

impl Cursor {
    #[inline]
    pub const fn current(&self) -> usize {
        self.current
    }

    #[inline]
    pub const fn anchored(&self) -> usize {
        self.anchor
    }

    pub const fn span(&self) -> Span {
        Span {
            start: self.anchor,
            end: self.current,
        }
    }

    pub fn anchor(&mut self) {
        self.anchor = self.current;
    }

    pub fn increment(&mut self, by: usize) {
        self.current += by;
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<Spanned<Token>, LexError>;

    fn next(&mut self) -> Option<Self::Item> {
        self.consume(|byte| byte.is_ascii_whitespace());

        self.cursor.anchor();

        macro just($kind:expr $(, $incr:literal)?) {{
            self.last = Some($kind);
            $(self.cursor.increment($incr);)?
            Some(Ok(self.finalize_token($kind)))
        }}

        let Some(byte) = self.peek() else {
            return match self.last {
                Some(TokenKind::Eof) => None,
                _ => just!(TokenKind::Eof),
            };
        };

        self.cursor.increment(1);

        enum Started {
            Identifier,
            Number,
            String,
            IfPeekEqElse(u8, TokenKind, TokenKind),
            Minus,
            Less,
            Greater,
            Amp,
            Pipe,
        }

        let started = match byte {
            b'=' => Started::IfPeekEqElse(b'=', TokenKind::EqEq, TokenKind::Eq),
            b'!' => Started::IfPeekEqElse(b'=', TokenKind::BangEq, TokenKind::Bang),
            b'a'..=b'z' | b'A'..=b'Z' | b'_' => Started::Identifier,
            b'0'..=b'9' => Started::Number,
            b'"' => Started::String,
            b'-' => Started::Minus,
            b'+' => Started::IfPeekEqElse(b'=', TokenKind::PlusEq, TokenKind::Plus),
            b'*' => Started::IfPeekEqElse(b'=', TokenKind::StarEq, TokenKind::Star),
            b'/' => Started::IfPeekEqElse(b'=', TokenKind::SlashEq, TokenKind::Slash),
            b'>' => Started::Greater,
            b'<' => Started::Less,
            b'&' => Started::Amp,
            b'^' => Started::IfPeekEqElse(b'=', TokenKind::CaretEq, TokenKind::Caret),
            b'|' => Started::Pipe,
            b'.' => return just!(TokenKind::Dot),
            b',' => return just!(TokenKind::Comma),
            b'(' => return just!(TokenKind::LParen),
            b')' => return just!(TokenKind::RParen),
            b'{' => return just!(TokenKind::LBrace),
            b'}' => return just!(TokenKind::RBrace),
            b';' => return just!(TokenKind::Semicolon),
            b':' => return just!(TokenKind::Colon),
            _ => {
                return Some(Err(SingleTokenError {
                    token: byte as char,
                    span: self.cursor.span(),
                }
                .into()));
            }
        };

        match started {
            Started::Identifier => {
                self.consume(|byte| matches!(byte, b'a'..=b'z' | b'A'..=b'Z' | b'_'));

                // keywords match as an identifier
                match self.spanned_bytes() {
                    b"let" => just!(TokenKind::KeywordLet),
                    b"fn" => just!(TokenKind::KeywordFn),
                    b"true" | b"false" => just!(TokenKind::LitBool),
                    _ => just!(TokenKind::Identifier),
                }
            }
            Started::Number => {
                let mut found_dot = false;

                self.consume(|byte| match byte {
                    b'.' if !found_dot => {
                        found_dot = true;
                        true
                    }
                    b'0'..=b'9' => true,
                    _ => false,
                });

                if found_dot {
                    just!(TokenKind::Float)
                } else {
                    just!(TokenKind::BigInt)
                }
            }
            Started::String => {
                self.consume(|byte| byte != b'"');

                if self.peek() != Some(b'"') {
                    return Some(Err(StringTerminationError {
                        span: self.cursor.span(),
                    }
                    .into()));
                }

                self.cursor.increment(1);

                just!(TokenKind::LitString)
            }
            Started::Minus => match self.peek() {
                Some(b'>') => just!(TokenKind::RArrow, 1),
                Some(b'=') => just!(TokenKind::MinusEq, 1),
                _ => just!(TokenKind::Minus),
            },
            Started::Greater => match self.peek_n::<2>() {
                Some([b'=', _]) => just!(TokenKind::GtEq, 1),
                Some([b'>', b'=']) => just!(TokenKind::ShrEq, 2),
                Some([b'>', _]) => just!(TokenKind::Shr, 1),
                _ => just!(TokenKind::Gt),
            },
            Started::Less => match self.peek_n::<2>() {
                Some([b'=', _]) => just!(TokenKind::LtEq, 1),
                Some([b'<', b'=']) => just!(TokenKind::ShlEq, 2),
                Some([b'<', _]) => just!(TokenKind::Shl, 1),
                _ => just!(TokenKind::Lt),
            },
            Started::Amp => match self.peek() {
                Some(b'=') => just!(TokenKind::AmpEq, 1),
                Some(b'&') => just!(TokenKind::AmpAmp, 1),
                _ => just!(TokenKind::Amp),
            },
            Started::Pipe => match self.peek() {
                Some(b'=') => just!(TokenKind::PipeEq, 1),
                Some(b'|') => just!(TokenKind::PipePipe, 1),
                _ => just!(TokenKind::Pipe),
            },
            Started::IfPeekEqElse(check, then, otherwise) => match self.peek() {
                Some(actual) if actual == check => just!(then, 1),
                _ => just!(otherwise),
            },
        }
    }
}

impl Lexer<'_> {
    #[inline]
    fn finalize_token(&self, kind: TokenKind) -> Spanned<Token> {
        Spanned {
            span: self.cursor.span(),
            item: match kind {
                TokenKind::KeywordLet => Token::KeywordLet,
                TokenKind::KeywordFn => Token::KeywordFn,
                TokenKind::Identifier => Token::Identifier(EcoString::from(self.spanned_symbol())),
                TokenKind::Plus => Token::Plus,
                TokenKind::PlusEq => Token::PlusEq,
                TokenKind::Minus => Token::Minus,
                TokenKind::MinusEq => Token::MinusEq,
                TokenKind::Star => Token::Star,
                TokenKind::StarEq => Token::StarEq,
                TokenKind::Slash => Token::Slash,
                TokenKind::SlashEq => Token::SlashEq,
                TokenKind::Eq => Token::Eq,
                TokenKind::EqEq => Token::EqEq,
                TokenKind::Bang => Token::Bang,
                TokenKind::BangEq => Token::BangEq,
                TokenKind::Gt => Token::Gt,
                TokenKind::GtEq => Token::GtEq,
                TokenKind::Shr => Token::Shr,
                TokenKind::ShrEq => Token::ShrEq,
                TokenKind::Lt => Token::Lt,
                TokenKind::LtEq => Token::LtEq,
                TokenKind::Shl => Token::Shl,
                TokenKind::ShlEq => Token::ShlEq,
                TokenKind::Amp => Token::Amp,
                TokenKind::AmpEq => Token::AmpEq,
                TokenKind::AmpAmp => Token::AmpAmp,
                TokenKind::Caret => Token::Caret,
                TokenKind::CaretEq => Token::CaretEq,
                TokenKind::Pipe => Token::Pipe,
                TokenKind::PipeEq => Token::PipeEq,
                TokenKind::PipePipe => Token::PipePipe,
                TokenKind::BigInt => Token::BigInt(
                    parse_number(self.spanned_symbol())
                        .expect("valid bigint representation to be parsed"),
                ),
                TokenKind::LitString => Token::LitString(EcoString::from(unsafe {
                    core::str::from_utf8_unchecked(
                        &self.bytes[self.cursor.anchor + 1..self.cursor.current - 1],
                    )
                })),
                TokenKind::LitBool => Token::LitBool(parse_boolean(self.spanned_bytes())),
                TokenKind::Comma => Token::Comma,
                TokenKind::Dot => Token::Dot,
                TokenKind::LParen => Token::LParen,
                TokenKind::RParen => Token::RParen,
                TokenKind::LBrace => Token::LBrace,
                TokenKind::RBrace => Token::RBrace,
                TokenKind::RArrow => Token::RArrow,
                TokenKind::Semicolon => Token::Semicolon,
                TokenKind::Colon => Token::Colon,
                TokenKind::Eof => Token::Eof,
                _ => todo!(),
            },
        }
    }
}

fn parse_boolean(bytes: &[u8]) -> bool {
    match bytes {
        b"true" => true,
        b"false" => false,
        _ => panic!("invalid boolean value {}", unsafe {
            core::str::from_utf8_unchecked(bytes)
        }),
    }
}

fn parse_number(symbol: &str) -> Option<BigInt> {
    BigInt::parse_bytes(symbol.as_bytes(), 10)
}
