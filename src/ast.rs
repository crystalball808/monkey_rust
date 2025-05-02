use crate::Token;

#[derive(Debug, PartialEq)]
/// Prefix operator (e.g. `-5`, `!foo`)
pub enum PrefixOperator {
    /// e.g. `!5`, `!foo(1, "bar")`
    Not,
    /// e.g. `-15`
    Negative,
}

#[derive(Debug, PartialEq)]
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

impl TryFrom<&Token<'_>> for InfixOperator {
    type Error = ();

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
            _ => Err(()),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Expression<'i> {
    IntLiteral(u32),
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
        Box<Vec<Statement<'i>>>,
        Box<Vec<Statement<'i>>>,
    ),
    Func(Vec<&'i str>, Vec<Statement<'i>>),
}

#[derive(Debug, PartialEq)]
pub enum Statement<'i> {
    Let(&'i str, Expression<'i>),
    Return(Expression<'i>),
    Expression(Expression<'i>),
}

#[derive(Debug, PartialEq)]
pub struct Program<'i> {
    statements: Vec<Statement<'i>>,
}

impl<'i> Program<'i> {
    pub fn new(statements: Vec<Statement<'i>>) -> Self {
        Self { statements }
    }
}
