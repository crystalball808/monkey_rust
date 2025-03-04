use crate::{Lexer, ast::Program};

struct Parser<'a> {
    lexer: Lexer<'a>,
}

impl<'l> Parser<'l> {
    fn new(lexer: Lexer<'l>) -> Self {
        Self { lexer }
    }
    fn parse_program(self) -> Result<Program<'l>, String> {
        todo!()
    }
}

#[test]
fn let_statement() {
    use crate::ast::{Expression, Statement};

    let input = "
let x = 5;
let y = 10;
let foobar = 838383";

    let lexer = Lexer::new(input);
    let parser = Parser::new(lexer);
    let parsed_ast = parser.parse_program().expect("Failed to parse the program");

    let expected_ast = Program::new(vec![
        Statement::LetStatement("x", Expression::Int(5)),
        Statement::LetStatement("y", Expression::Int(10)),
        Statement::LetStatement("foobar", Expression::Int(838383)),
    ]);

    assert_eq!(parsed_ast, expected_ast);
}
