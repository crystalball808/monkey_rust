use crate::{
    Lexer, Token,
    ast::{Expression, Program, Statement},
};

struct Parser<'a> {
    lexer: Lexer<'a>,
}

impl<'l> Parser<'l> {
    fn new(lexer: Lexer<'l>) -> Self {
        Self { lexer }
    }
    fn parse_program(self) -> Result<Program<'l>, String> {
        let mut lexer = self.lexer.peekable();

        let mut statements: Vec<Statement> = Vec::new();
        while let Some(token) = lexer.next() {
            match token {
                Token::Let => {
                    let name = match lexer
                        .next()
                        .ok_or(String::from("Let statement not finished"))?
                    {
                        Token::Identifier(ident) => ident,
                        _ => return Err(String::from("Illegal synthax")),
                    };

                    let Some(Token::Assign) = lexer.next() else {
                        return Err(String::from("Let statement without the assign token"));
                    };

                    let expr = match lexer
                        .next()
                        .ok_or(String::from("Let statement not finished"))?
                    {
                        Token::Int(integer) => Expression::Int(integer),
                        _ => return Err(String::from("Illegal synthax")),
                    };

                    statements.push(Statement::LetStatement(name, expr));

                    let Some(Token::Semicolon) = lexer.next() else {
                        return Err(String::from("Let statement without the semicolon"));
                    };
                }
                _ => {}
            }
        }

        Ok(Program::new(statements))
    }
}

#[test]
fn let_statement() {
    use crate::ast::{Expression, Statement};

    let input = "
let x = 5;
let y = 10;
let foobar = 838383;";

    let lexer = Lexer::new(input);
    let parser = Parser::new(lexer);
    let parsed_ast = parser
        .parse_program()
        .expect("Should be parsed successfully");

    let expected_ast = Program::new(vec![
        Statement::LetStatement("x", Expression::Int(5)),
        Statement::LetStatement("y", Expression::Int(10)),
        Statement::LetStatement("foobar", Expression::Int(838383)),
    ]);

    assert_eq!(parsed_ast, expected_ast);
}
