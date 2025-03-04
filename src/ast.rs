#[derive(Debug, PartialEq)]
pub enum Expression {
    Int(u32),
}

#[derive(Debug, PartialEq)]
pub enum Statement<'i> {
    LetStatement(&'i str, Expression),
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
