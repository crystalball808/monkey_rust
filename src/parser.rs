use std::iter::Peekable;

use crate::{
    Lexer, Token,
    ast::{Expression, InfixOperator, PrefixOperator, Program, Statement},
};

struct Parser<'l> {
    lexer: Peekable<Lexer<'l>>,
}

impl<'l> Parser<'l> {
    #[allow(dead_code)]
    fn new(lexer: Lexer<'l>) -> Self {
        Self {
            lexer: lexer.peekable(),
        }
    }
    // fn prefix_parse() -> Expression<'l> {
    //     todo!()
    // }
    // fn infix_parse(
    //     &mut self,
    //     left_expr: Expression<'l>,
    //     operator: InfixOperator,
    // ) -> Expression<'l> {
    //     todo!()
    // }
    fn parse_expression(&mut self) -> Result<Expression<'l>, String> {
        let expr = match self.lexer.next().ok_or(String::from("No token to parse"))? {
            Token::Int(integer) => {
                let left_expr = Expression::IntLiteral(integer);
                let Some(peeked_token) = self.lexer.peek() else {
                    return Ok(left_expr);
                };

                if let Ok(infix_operator) = InfixOperator::try_from(peeked_token) {
                    self.lexer.next();
                    let rigth_expr = self.parse_expression()?;

                    Ok(Expression::Infix(
                        infix_operator,
                        Box::new(left_expr),
                        Box::new(rigth_expr),
                    ))
                } else {
                    Ok(left_expr)
                }
            }
            Token::Identifier(identifier) => Ok(Expression::Identifier(identifier)),
            Token::Minus => {
                let expr = Expression::Prefix(
                    PrefixOperator::Negative,
                    Box::new(self.parse_expression()?),
                );

                Ok(expr)
            }
            Token::Bang => {
                let expr =
                    Expression::Prefix(PrefixOperator::Not, Box::new(self.parse_expression()?));

                Ok(expr)
            }
            other_token => Err(String::from(format!("Unexpected token: {:?}", other_token))),
        };

        expr
    }
    #[allow(dead_code)]
    fn parse_program(mut self) -> Result<Program<'l>, String> {
        let mut statements: Vec<Statement> = Vec::new();
        while let Some(token) = self.lexer.peek() {
            match token {
                Token::Let => {
                    self.lexer.next();
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
                    self.lexer.next();
                    let expr = self.parse_expression()?;

                    statements.push(Statement::Return(expr));

                    let Some(Token::Semicolon) = self.lexer.next() else {
                        return Err(String::from("Statement without the semicolon"));
                    };
                }
                _ => {
                    let expr = self.parse_expression()?;

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

#[test]
fn prefix_expression() {
    let input = "-5;
-foobar;
!10;
!x;";

    let lexer = Lexer::new(input);
    let parser = Parser::new(lexer);
    let parsed_ast = parser
        .parse_program()
        .expect("Should be parsed successfully");

    let expected_ast = Program::new(vec![
        Statement::Expression(Expression::Prefix(
            PrefixOperator::Negative,
            Box::new(Expression::IntLiteral(5)),
        )),
        Statement::Expression(Expression::Prefix(
            PrefixOperator::Negative,
            Box::new(Expression::Identifier("foobar")),
        )),
        Statement::Expression(Expression::Prefix(
            PrefixOperator::Not,
            Box::new(Expression::IntLiteral(10)),
        )),
        Statement::Expression(Expression::Prefix(
            PrefixOperator::Not,
            Box::new(Expression::Identifier("x")),
        )),
    ]);

    assert_eq!(parsed_ast, expected_ast);
}

#[test]
fn infix_precedence() {
    let input = "
5 + 10 / 2;
10 / 2 + 5;
10 - 2 < 7 + 3;
";
    let lexer = Lexer::new(input);
    let parser = Parser::new(lexer);
    let parsed_ast = parser
        .parse_program()
        .expect("Should be parsed successfully");

    let expected_ast = Program::new(vec![
        Statement::Expression(Expression::Infix(
            InfixOperator::Add,
            Box::new(Expression::IntLiteral(5)),
            Box::new(Expression::Infix(
                InfixOperator::Divide,
                Box::new(Expression::IntLiteral(10)),
                Box::new(Expression::IntLiteral(2)),
            )),
        )),
        Statement::Expression(Expression::Infix(
            InfixOperator::Add,
            Box::new(Expression::IntLiteral(5)),
            Box::new(Expression::Infix(
                InfixOperator::Divide,
                Box::new(Expression::IntLiteral(10)),
                Box::new(Expression::IntLiteral(2)),
            )),
        )),
        Statement::Expression(Expression::Infix(
            InfixOperator::LessThan,
            Box::new(Expression::Infix(
                InfixOperator::Subtract,
                Box::new(Expression::IntLiteral(10)),
                Box::new(Expression::IntLiteral(2)),
            )),
            Box::new(Expression::Infix(
                InfixOperator::Add,
                Box::new(Expression::IntLiteral(7)),
                Box::new(Expression::IntLiteral(3)),
            )),
        )),
    ]);

    assert_eq!(parsed_ast, expected_ast);
}

#[test]
fn infix_expression() {
    let input = "
5 + 5;
5 - 5;
5 * 5;
5 / 5;
5 > 5;
5 < 5;
5 == 5;
5 != 5;
";
    let lexer = Lexer::new(input);
    let parser = Parser::new(lexer);
    let parsed_ast = parser
        .parse_program()
        .expect("Should be parsed successfully");

    let expected_ast = Program::new(vec![
        Statement::Expression(Expression::Infix(
            InfixOperator::Add,
            Box::new(Expression::IntLiteral(5)),
            Box::new(Expression::IntLiteral(5)),
        )),
        Statement::Expression(Expression::Infix(
            InfixOperator::Subtract,
            Box::new(Expression::IntLiteral(5)),
            Box::new(Expression::IntLiteral(5)),
        )),
        Statement::Expression(Expression::Infix(
            InfixOperator::Multiply,
            Box::new(Expression::IntLiteral(5)),
            Box::new(Expression::IntLiteral(5)),
        )),
        Statement::Expression(Expression::Infix(
            InfixOperator::Divide,
            Box::new(Expression::IntLiteral(5)),
            Box::new(Expression::IntLiteral(5)),
        )),
        Statement::Expression(Expression::Infix(
            InfixOperator::GreaterThan,
            Box::new(Expression::IntLiteral(5)),
            Box::new(Expression::IntLiteral(5)),
        )),
        Statement::Expression(Expression::Infix(
            InfixOperator::LessThan,
            Box::new(Expression::IntLiteral(5)),
            Box::new(Expression::IntLiteral(5)),
        )),
        Statement::Expression(Expression::Infix(
            InfixOperator::Equals,
            Box::new(Expression::IntLiteral(5)),
            Box::new(Expression::IntLiteral(5)),
        )),
        Statement::Expression(Expression::Infix(
            InfixOperator::NotEquals,
            Box::new(Expression::IntLiteral(5)),
            Box::new(Expression::IntLiteral(5)),
        )),
    ]);
    assert_eq!(parsed_ast, expected_ast);
}
