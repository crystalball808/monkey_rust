#[derive(Debug, PartialEq)]
/// Prefix operator (e.g. `-5`, `!foo`)
pub enum PrefixOperator {
    /// e.g. `!5`, `!foo(1, "bar")`
    Not,
    /// e.g. `-15`
    Negative,
}

#[derive(Debug, PartialEq)]
pub enum Expression<'i> {
    IntLiteral(u32),
    Identifier(&'i str),
    Prefix(PrefixOperator, Box<Expression<'i>>),
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
