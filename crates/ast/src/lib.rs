use std::rc::Rc;

use ecow::EcoString;
use untyped::Expression;

pub mod typed;
pub mod untyped;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Statement {
    Expression(Expression),
    Assignment(Assignment),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Assignment {
    pub value: Rc<Expression>,
    pub name: EcoString,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Arg {
    pub symbol: EcoString,
}
