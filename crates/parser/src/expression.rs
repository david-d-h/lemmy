use std::rc::Rc;

use ast::untyped as ast;
use ast::*;

use crate::combinator::*;
use crate::token::Token;
use crate::token::TokenKind;
use crate::{Parser, Result};

#[inline]
pub fn bp<T: HasBindingPower>(of: T) -> (T::Lhs, T::Rhs) {
    of.binding_power()
}

/// Parse an expression with operator and operator presedence in mind.
pub fn expression(
    parser: &mut Parser<'_>,
    stack: Option<Expression>,
    power: u8,
) -> Result<Expression> {
    let peek = parser.peek_kind();

    match stack {
        // parse an invocation of a lhs expression
        Some(lhs) if let Some(TokenKind::LParen) = peek => {
            let stack = Some(invocation(parser, lhs)?);
            expression(parser, stack, power)
        },
        Some(lhs) // finish a bound postfix expression and recurse
            if let Some(op) = peek.map(to_postfix_op).flatten()
                && let (lhs_bp, ()) = bp(op)
                && lhs_bp >= power =>
        {
            parser
                .advance()
                .expect("a postfix unary operator was just peeked");
            expression(parser, Some(Expression::Postfix(Rc::new(lhs), op)), power)
        },
        Some(lhs) // parse rhs of binary operator expression, finish, and recurse
            if let Some(op) = peek.map(to_bin_op).flatten()
                && let (lhs_bp, rhs_bp) = bp(op)
                && lhs_bp >= power =>
        {
            parser.advance().expect("a binary operator was just peeked");
            let unconcerned_rhs = unconcerned_expr(parser)?;
            let rhs = expression(parser, Some(unconcerned_rhs), rhs_bp)?;
            expression(parser, Some(Expression::Binary(Rc::new(lhs), op, Rc::new(rhs))), power)
        },
        // a prefix operator was found, parse a rhs expression and recurse
        None if let Some(op) = peek.map(to_prefix_op).flatten() => {
            parser
                .advance()
                .expect("a prefix unary operator was just peeked");
            let ((), rhs_bp) = bp(op);
            let rhs = expression(parser, None, rhs_bp)?;
            expression(parser, Some(Expression::Prefix(op, Rc::new(rhs))), power)
        },
        Some(expr) => Ok(expr), // finished parsing a singular expression
        None => { // start parsing a singular expression that may (or may not) include operators
            let unconcerned = unconcerned_expr(parser)?;
            expression(parser, Some(unconcerned), power)
        },
    }
}

pub fn unconcerned_expr(parser: &mut Parser<'_>) -> Result<Expression> {
    let token = parser.peek_one_of([
        TokenKind::Identifier,
        TokenKind::BigInt,
        TokenKind::LitString,
        TokenKind::LitBool,
        TokenKind::LParen,
        TokenKind::LBrace,
        TokenKind::KeywordFn,
    ])?;

    let expr = match token.into_inner() {
        Token::Identifier(symbol) => Expression::Identifier(symbol),
        Token::BigInt(value) => Expression::Int(value),
        Token::LitString(value) => Expression::String(value),
        Token::LitBool(value) => Expression::Boolean(value),
        Token::LParen => return group(parser),
        Token::LBrace => return block(parser),
        Token::KeywordFn => return function(parser),
        _ => unreachable!(),
    };

    _ = parser
        .advance()
        .expect("peek was successful, so advance should be too?");

    Ok(expr)
}

pub fn group(parser: &mut Parser<'_>) -> Result<Expression> {
    delimited(TokenKind::LParen, any_expression, TokenKind::RParen)(parser)
        .map(Rc::new)
        .map(Expression::Group)
}

pub fn block(parser: &mut Parser<'_>) -> Result<Expression> {
    delimited(
        TokenKind::LBrace,
        many(maybe(crate::statement::statement)),
        TokenKind::RBrace,
    )(parser)
    .map(Rc::new)
    .map(Expression::Block)
}

pub fn function(parser: &mut Parser<'_>) -> Result<Expression> {
    Ok(Expression::Function {
        arguments: prefixed([TokenKind::KeywordFn], crate::common::args)(parser)?,
        body: Rc::new(any_expression(parser)?),
    })
}

pub fn invocation(parser: &mut Parser<'_>, on: Expression) -> Result<Expression> {
    delimited(
        TokenKind::LParen,
        separated(any_expression, TokenKind::Comma),
        TokenKind::RParen,
    )(parser)
    .map(move |arguments| Expression::Invocation {
        expr: Rc::new(on),
        arguments,
    })
}

#[cfg(test)]
mod tests {
    use crate::lexer::Lexer;

    use super::*;

    fn expression(src: &'static str) -> Expression {
        any_expression(&mut Parser {
            lexer: Lexer::new(src),
        })
        .expect("source should consist of a valid expression")
    }

    crate::insta_tests![
        [compact] string as expression { "Hallo!" }

        [compact] number as expression { 1 }

        [compact] boolean as expression { true }

        [compact] ident as expression { hallo_ }

        [compact] grouped_ident as expression { (hal_lo_) }

        nested_groups as expression { (((a))) }

        [compact] one_plus_one as expression { 1 + 1 }

        [compact] accessor as expression { person.name }

        [compact] presedence_mul_over_plus as expression { 1 + 1 * 2 }

        [compact] negative_number_literal as expression { -12 }

        [compact] invocation_of_ident as expression { real(1, 2, 3) }

        [compact] invocation_of_ident_with_trailing_comma_in_argument_list as expression { real(1, 2,) }

        block_many_expr as expression {
            {
                io.print("harharhar")
                io.debug(3, 2, 1)
            }
        }

        [compact] simple_function as expression { fn(x) x }

        simple_function_with_block as expression { fn(x) { x } }
    ];
}

pub const fn to_bin_op(kind: TokenKind) -> Option<BinOp> {
    match kind {
        TokenKind::Plus => Some(BinOp::Plus),
        TokenKind::PlusEq => Some(BinOp::PlusEq),
        TokenKind::Minus => Some(BinOp::Minus),
        TokenKind::MinusEq => Some(BinOp::MinusEq),
        TokenKind::Star => Some(BinOp::Star),
        TokenKind::StarEq => Some(BinOp::StarEq),
        TokenKind::Slash => Some(BinOp::Slash),
        TokenKind::SlashEq => Some(BinOp::SlashEq),
        TokenKind::EqEq => Some(BinOp::EqEq),
        TokenKind::BangEq => Some(BinOp::Neq),
        TokenKind::Gt => Some(BinOp::Gt),
        TokenKind::GtEq => Some(BinOp::GtEq),
        TokenKind::Shr => Some(BinOp::ShrEq),
        TokenKind::Lt => Some(BinOp::Lt),
        TokenKind::LtEq => Some(BinOp::LtEq),
        TokenKind::Shl => Some(BinOp::Shl),
        TokenKind::ShlEq => Some(BinOp::ShlEq),
        TokenKind::Amp => Some(BinOp::Amp),
        TokenKind::AmpEq => Some(BinOp::AmpEq),
        TokenKind::AmpAmp => Some(BinOp::And),
        TokenKind::Caret => Some(BinOp::Caret),
        TokenKind::CaretEq => Some(BinOp::CaretEq),
        TokenKind::Pipe => Some(BinOp::Pipe),
        TokenKind::PipeEq => Some(BinOp::PipeEq),
        TokenKind::PipePipe => Some(BinOp::Or),
        TokenKind::Dot => Some(BinOp::Dot),
        _ => None,
    }
}

pub const fn to_prefix_op(kind: TokenKind) -> Option<PrefixOp> {
    match kind {
        TokenKind::Plus => Some(PrefixOp::Plus),
        TokenKind::Minus => Some(PrefixOp::Minus),
        TokenKind::Bang => Some(PrefixOp::Bang),
        _ => None,
    }
}

pub const fn to_postfix_op(kind: TokenKind) -> Option<PostfixOp> {
    match kind {
        _ => None,
    }
}
