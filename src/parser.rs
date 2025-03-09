use crate::{
    Lexer, Token,
    ast::{Expression, Program, Statement},
};

struct Parser<'l> {
    lexer: Lexer<'l>,
}

impl<'l> Parser<'l> {
    #[allow(dead_code)]
    fn new(lexer: Lexer<'l>) -> Self {
        Self { lexer }
    }
    // fn prefix_parse() -> Expression<'l> {
    //     todo!()
    // }
    // fn infix_parse() -> Expression<'l> {
    //     todo!()
    // }
    fn parse_expression(&mut self) -> Result<Expression<'l>, String> {
        let expr = match self.lexer.next().ok_or(String::from("No token to parse"))? {
            Token::Int(integer) => Ok(Expression::IntLiteral(integer)),
            Token::Identifier(identifier) => Ok(Expression::Identifier(identifier)),
            other_token => Err(String::from(format!("Unexpected token: {:?}", other_token))),
        };

        expr
    }
    #[allow(dead_code)]
    fn parse_program(mut self) -> Result<Program<'l>, String> {
        let mut statements: Vec<Statement> = Vec::new();
        while let Some(token) = self.lexer.next() {
            match token {
                Token::Let => {
                    let name = match self
                        .lexer
                        .next()
                        .ok_or(String::from("Let statement not finished"))?
                    {
                        Token::Identifier(ident) => ident,
                        _ => return Err(String::from("Illegal synthax")),
                    };

                    let Some(Token::Assign) = self.lexer.next() else {
                        return Err(String::from("Let statement without the assign token"));
                    };

                    let expr = self.parse_expression()?;

                    statements.push(Statement::Let(name, expr));

                    let Some(Token::Semicolon) = self.lexer.next() else {
                        return Err(String::from("Let statement without the semicolon"));
                    };
                }
                Token::Return => {
                    let expr = self.parse_expression()?;

                    statements.push(Statement::Return(expr));

                    let Some(Token::Semicolon) = self.lexer.next() else {
                        return Err(String::from("Statement without the semicolon"));
                    };
                }
                other_token => {
                    let expr = match other_token {
                        Token::Int(integer) => Expression::IntLiteral(integer),
                        Token::Identifier(identifier) => Expression::Identifier(identifier),
                        other_token => {
                            return Err(String::from(format!(
                                "Unexpected token: {:?}",
                                other_token
                            )));
                        }
                    };

                    statements.push(Statement::Expression(expr));

                    let Some(Token::Semicolon) = self.lexer.next() else {
                        return Err(String::from("Statement without the semicolon"));
                    };
                }
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
        Statement::Let("x", Expression::IntLiteral(5)),
        Statement::Let("y", Expression::IntLiteral(10)),
        Statement::Let("foobar", Expression::IntLiteral(838383)),
    ]);

    assert_eq!(parsed_ast, expected_ast);
}
#[test]
fn return_statement() {
    let input = "
return 5;
return 993322;
return foobar;
";
    let lexer = Lexer::new(input);
    let parser = Parser::new(lexer);
    let parsed_ast = parser
        .parse_program()
        .expect("Should be parsed successfully");

    let expected_ast = Program::new(vec![
        Statement::Return(Expression::IntLiteral(5)),
        Statement::Return(Expression::IntLiteral(993322)),
        Statement::Return(Expression::Identifier("foobar")),
    ]);

    assert_eq!(parsed_ast, expected_ast);
}

#[test]
fn expression_statement() {
    let input = "
foobar;
5;";

    let lexer = Lexer::new(input);
    let parser = Parser::new(lexer);
    let parsed_ast = parser
        .parse_program()
        .expect("Should be parsed successfully");

    let expected_ast = Program::new(vec![
        Statement::Expression(Expression::Identifier("foobar")),
        Statement::Expression(Expression::IntLiteral(5)),
    ]);

    assert_eq!(parsed_ast, expected_ast);
}
