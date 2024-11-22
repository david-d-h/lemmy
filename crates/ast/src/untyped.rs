use std::rc::Rc;

use ecow::EcoString;

use num_bigint::BigInt;

use crate::{Arg, Statement};

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Expression {
    String(EcoString),
    Int(BigInt),
    Boolean(bool),
    Identifier(EcoString),
    Group(Rc<Expression>),
    Binary(Rc<Expression>, BinOp, Rc<Expression>),
    Prefix(PrefixOp, Rc<Expression>),
    Postfix(Rc<Expression>, PostfixOp),
    Invocation {
        expr: Rc<Expression>,
        arguments: Vec<Expression>,
    },
    Block(Rc<Vec<Statement>>),
    Function {
        arguments: Vec<Arg>,
        body: Rc<Expression>,
    },
}

pub trait HasBindingPower {
    type Lhs;
    type Rhs;

    fn binding_power(&self) -> (Self::Lhs, Self::Rhs);
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum BinOp {
    Plus,
    PlusEq,
    Minus,
    MinusEq,
    Star,
    StarEq,
    Slash,
    SlashEq,
    EqEq,
    Neq,
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
    And,
    Caret,
    CaretEq,
    Pipe,
    PipeEq,
    Or,
    Dot,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum PrefixOp {
    Minus,
    Plus,
    Bang,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum PostfixOp {
    // ...
}

impl HasBindingPower for BinOp {
    type Lhs = u8;
    type Rhs = u8;

    #[inline]
    fn binding_power(&self) -> (u8, u8) {
        match self {
            BinOp::Dot => (24, 23),
            BinOp::Star | BinOp::Slash => (19, 20),
            BinOp::Plus | BinOp::Minus => (17, 18),
            BinOp::Shl | BinOp::Shr => (15, 16),
            BinOp::Amp => (13, 14),
            BinOp::Caret => (11, 12),
            BinOp::Pipe => (9, 10),
            BinOp::EqEq | BinOp::Neq | BinOp::Lt | BinOp::Gt | BinOp::LtEq | BinOp::GtEq => (7, 8),
            BinOp::And => (5, 6),
            BinOp::Or => (3, 4),
            BinOp::PlusEq
            | BinOp::MinusEq
            | BinOp::StarEq
            | BinOp::SlashEq
            | BinOp::ShlEq
            | BinOp::ShrEq
            | BinOp::AmpEq
            | BinOp::CaretEq
            | BinOp::PipeEq => (1, 2),
        }
    }
}

impl HasBindingPower for PrefixOp {
    type Lhs = ();
    type Rhs = u8;

    #[inline]
    fn binding_power(&self) -> ((), u8) {
        match self {
            PrefixOp::Plus | PrefixOp::Minus | PrefixOp::Bang => ((), 21),
        }
    }
}

impl HasBindingPower for PostfixOp {
    type Lhs = u8;
    type Rhs = ();

    #[inline]
    fn binding_power(&self) -> (u8, ()) {
        match self {
            _ => unreachable!(),
        }
    }
}
