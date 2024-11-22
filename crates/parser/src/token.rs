use core::fmt;

use ecow::EcoString;
use num_bigint::BigInt;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum TokenKind {
    KeywordLet,
    KeywordFn,
    Identifier,
    Plus,
    PlusEq,
    Minus,
    MinusEq,
    Star,
    StarEq,
    Slash,
    SlashEq,
    Eq,
    EqEq,
    Bang,
    BangEq,
    Gt,
    GtEq,
    Shr,
    ShrEq,
    Lt,
    LtEq,
    Shl,
    ShlEq,
    Amp,
    AmpEq,
    AmpAmp,
    Caret,
    CaretEq,
    Pipe,
    PipeEq,
    PipePipe,
    BigInt,
    Float,
    LitString,
    LitBool,
    Comma,
    Dot,
    LParen,
    RParen,
    LBrace,
    RBrace,
    RArrow,
    Semicolon,
    Colon,
    Eof,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Token {
    KeywordLet,
    KeywordFn,
    Identifier(EcoString),
    Plus,
    PlusEq,
    Minus,
    MinusEq,
    Star,
    StarEq,
    Slash,
    SlashEq,
    Eq,
    EqEq,
    Bang,
    BangEq,
    Gt,
    GtEq,
    Shr,
    ShrEq,
    Lt,
    LtEq,
    Shl,
    ShlEq,
    Amp,
    AmpEq,
    AmpAmp,
    Caret,
    CaretEq,
    Pipe,
    PipeEq,
    PipePipe,
    BigInt(BigInt),
    LitString(EcoString),
    LitBool(bool),
    Comma,
    Dot,
    LParen,
    RParen,
    LBrace,
    RBrace,
    RArrow,
    Semicolon,
    Colon,
    Eof,
}

impl Token {
    #[inline]
    pub fn kind(&self) -> TokenKind {
        token_to_kind(self)
    }
}

pub fn token_to_kind(token: &Token) -> TokenKind {
    match token {
        Token::KeywordLet => TokenKind::KeywordLet,
        Token::KeywordFn => TokenKind::KeywordFn,
        Token::Identifier(..) => TokenKind::Identifier,
        Token::Plus => TokenKind::Plus,
        Token::PlusEq => TokenKind::PlusEq,
        Token::Minus => TokenKind::Minus,
        Token::MinusEq => TokenKind::MinusEq,
        Token::Star => TokenKind::Star,
        Token::StarEq => TokenKind::StarEq,
        Token::Slash => TokenKind::Slash,
        Token::SlashEq => TokenKind::SlashEq,
        Token::Eq => TokenKind::Eq,
        Token::EqEq => TokenKind::EqEq,
        Token::Bang => TokenKind::Bang,
        Token::BangEq => TokenKind::BangEq,
        Token::Gt => TokenKind::Gt,
        Token::GtEq => TokenKind::GtEq,
        Token::Shr => TokenKind::Shr,
        Token::ShrEq => TokenKind::ShrEq,
        Token::Lt => TokenKind::Lt,
        Token::LtEq => TokenKind::LtEq,
        Token::Shl => TokenKind::Shl,
        Token::ShlEq => TokenKind::ShlEq,
        Token::Amp => TokenKind::Amp,
        Token::AmpEq => TokenKind::AmpEq,
        Token::AmpAmp => TokenKind::AmpAmp,
        Token::Caret => TokenKind::Caret,
        Token::CaretEq => TokenKind::CaretEq,
        Token::Pipe => TokenKind::Pipe,
        Token::PipeEq => TokenKind::PipeEq,
        Token::PipePipe => TokenKind::PipePipe,
        Token::BigInt { .. } => TokenKind::BigInt,
        Token::LitString(_) => TokenKind::LitString,
        Token::LitBool(_) => TokenKind::LitBool,
        Token::Comma => TokenKind::Comma,
        Token::Dot => TokenKind::Dot,
        Token::LParen => TokenKind::LParen,
        Token::RParen => TokenKind::RParen,
        Token::LBrace => TokenKind::LBrace,
        Token::RBrace => TokenKind::RBrace,
        Token::RArrow => TokenKind::RArrow,
        Token::Semicolon => TokenKind::Semicolon,
        Token::Colon => TokenKind::Colon,
        Token::Eof => TokenKind::Eof,
    }
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            TokenKind::KeywordLet => "let",
            TokenKind::KeywordFn => "fn",
            TokenKind::Identifier => "<ident>",
            TokenKind::Plus => "+",
            TokenKind::PlusEq => "+=",
            TokenKind::Minus => "-",
            TokenKind::MinusEq => "-=",
            TokenKind::Star => "*",
            TokenKind::StarEq => "*=",
            TokenKind::Slash => "/",
            TokenKind::SlashEq => "/=",
            TokenKind::Eq => "=",
            TokenKind::EqEq => "==",
            TokenKind::Bang => "!",
            TokenKind::BangEq => "!=",
            TokenKind::Gt => ">",
            TokenKind::GtEq => ">=",
            TokenKind::Shr => ">>",
            TokenKind::ShrEq => ">>=",
            TokenKind::Lt => "<",
            TokenKind::LtEq => "<=",
            TokenKind::Shl => "<<",
            TokenKind::ShlEq => "<<=",
            TokenKind::Amp => "&",
            TokenKind::AmpEq => "&=",
            TokenKind::AmpAmp => "&&",
            TokenKind::Caret => "^",
            TokenKind::CaretEq => "^=",
            TokenKind::Pipe => "|",
            TokenKind::PipeEq => "|=",
            TokenKind::PipePipe => "||",
            TokenKind::BigInt => "<number>",
            TokenKind::Float => "<float>",
            TokenKind::LitString => "<string>",
            TokenKind::LitBool => "<bool>",
            TokenKind::Comma => ",",
            TokenKind::Dot => ".",
            TokenKind::LParen => "(",
            TokenKind::RParen => ")",
            TokenKind::LBrace => "{",
            TokenKind::RBrace => "}",
            TokenKind::RArrow => "->",
            TokenKind::Semicolon => ";",
            TokenKind::Colon => ":",
            TokenKind::Eof => "<EOF>",
        })
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Token::Identifier(symbol) => symbol.as_str(),
            Token::BigInt(value) => return f.write_str(&value.to_string()),
            Token::LitString(value) => value.as_str(),
            Token::LitBool(value) => {
                if *value {
                    "true"
                } else {
                    "false"
                }
            }
            _ => return token_to_kind(self).fmt(f),
        })
    }
}
