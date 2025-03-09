#[derive(Debug, PartialEq)]
pub enum Expression {
    IntLiteral(u32),
}

#[derive(Debug, PartialEq)]
pub enum Statement<'i> {
    Let(&'i str, Expression),
    Return(Expression),
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
