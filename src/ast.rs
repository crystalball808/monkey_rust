struct Expression {}

enum Statement<'i> {
    LetStatement(&'i str, Expression),
}

struct Program<'i> {
    statements: Vec<Statement<'i>>,
}
