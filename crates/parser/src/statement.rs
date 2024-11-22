use ast::Statement;

use crate::combinator::*;
use crate::{Parser, Result};

pub fn statement(parser: &mut Parser<'_>) -> Result<Statement> {
    Ok(match parser.try_peek_kind()? {
        _ => Statement::Expression(any_expression(parser)?),
    })
}
