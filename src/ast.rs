use std::fmt::Display;

use crate::Token;

#[derive(Debug, Clone, PartialEq, PartialOrd)]
/// Prefix operator (e.g. `-5`, `!foo`)
pub enum PrefixOperator {
    /// e.g. `!5`, `!foo(1, "bar")`
    Not,
    /// e.g. `-15`
    Negative,
}
impl Display for PrefixOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PrefixOperator::Not => write!(f, "!"),
            PrefixOperator::Negative => write!(f, "-"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum InfixOperator {
    Equals,
    NotEquals,
    Add,
    Subtract,
    Multiply,
    Divide,
    GreaterThan,
    LessThan,
}
impl Display for InfixOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            InfixOperator::Equals => write!(f, "=="),
            InfixOperator::NotEquals => write!(f, "!="),
            InfixOperator::Add => write!(f, "+"),
            InfixOperator::Subtract => write!(f, "-"),
            InfixOperator::Multiply => write!(f, "*"),
            InfixOperator::Divide => write!(f, "+"),
            InfixOperator::GreaterThan => write!(f, ">"),
            InfixOperator::LessThan => write!(f, "<"),
        }
    }
}

// TODO: Implement Ord for InfixOperator
impl InfixOperator {
    pub fn get_precedence(&self) -> u8 {
        match self {
            InfixOperator::Equals => 0,
            InfixOperator::NotEquals => 0,
            InfixOperator::GreaterThan => 1,
            InfixOperator::LessThan => 1,
            InfixOperator::Add => 2,
            InfixOperator::Subtract => 2,
            InfixOperator::Multiply => 3,
            InfixOperator::Divide => 3,
        }
    }
}

pub struct InvalidInfixOperator;
impl TryFrom<&Token<'_>> for InfixOperator {
    type Error = InvalidInfixOperator;

    fn try_from(token: &Token<'_>) -> Result<Self, Self::Error> {
        match token {
            Token::LessThan => Ok(InfixOperator::LessThan),
            Token::GreaterThan => Ok(InfixOperator::GreaterThan),
            Token::Equals => Ok(InfixOperator::Equals),
            Token::NotEquals => Ok(InfixOperator::NotEquals),
            Token::Plus => Ok(InfixOperator::Add),
            Token::Minus => Ok(InfixOperator::Subtract),
            Token::Asterisk => Ok(InfixOperator::Multiply),
            Token::Slash => Ok(InfixOperator::Divide),
            _ => Err(InvalidInfixOperator),
        }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Expression<'i> {
    IntLiteral(i32),
    Boolean(bool),
    Identifier(&'i str),
    Prefix(PrefixOperator, Box<Expression<'i>>),
    Infix(
        InfixOperator,
        Box<Expression<'i>>,
        Box<Expression<'i>>,
        bool,
    ),
    If(
        Box<Expression<'i>>,
        Vec<Statement<'i>>,
        Option<Vec<Statement<'i>>>,
    ),
    Func(Vec<&'i str>, Vec<Statement<'i>>),
    Call(Box<Expression<'i>>, Vec<Expression<'i>>),
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Statement<'i> {
    Let(&'i str, Expression<'i>),
    Return(Expression<'i>),
    Expression(Expression<'i>),
}

#[derive(Debug, PartialEq)]
pub struct Program<'i> {
    pub statements: Vec<Statement<'i>>,
}

impl<'i> Program<'i> {
    pub fn new(statements: Vec<Statement<'i>>) -> Self {
        Self { statements }
    }
}
