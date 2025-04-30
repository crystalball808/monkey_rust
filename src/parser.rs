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
    fn infix_parse(
        &mut self,
        left_expr: Expression<'l>,
        boosted: bool,
    ) -> Result<Expression<'l>, String> {
        let infix_operator = InfixOperator::try_from(&self.lexer.next().expect("Should be"))
            .expect("Should be valid infix operator");

        if let Expression::Infix(left_infix_operator, _, _, false) = &left_expr {
            if left_infix_operator.get_precedence() > infix_operator.get_precedence() {
                let right_expr = self.parse_single_expression()?;
                return Ok(Expression::Infix(
                    infix_operator,
                    Box::new(left_expr),
                    Box::new(right_expr),
                    boosted,
                ));
            } else {
                let Expression::Infix(left_infix_operator, ll, lr, false) = left_expr else {
                    panic!("We checked this previously")
                };
                return Ok(Expression::Infix(
                    left_infix_operator,
                    ll,
                    Box::new(Expression::Infix(
                        infix_operator,
                        lr,
                        Box::new(self.parse_single_expression()?),
                        false,
                    )),
                    boosted,
                ));
            }
        }

        Ok(Expression::Infix(
            infix_operator,
            Box::new(left_expr),
            Box::new(self.parse_single_expression()?),
            boosted,
        ))
    }
    fn parse_single_expression(&mut self) -> Result<Expression<'l>, String> {
        let expr: Expression = match self.lexer.next().ok_or(String::from("No token to parse"))? {
            Token::Int(integer) => Expression::IntLiteral(integer),
            Token::False => Expression::Boolean(false),
            Token::True => Expression::Boolean(true),
            Token::Identifier(identifier) => Expression::Identifier(identifier),
            Token::Minus => {
                Expression::Prefix(PrefixOperator::Negative, Box::new(self.parse_expression()?))
            }
            Token::Bang => {
                Expression::Prefix(PrefixOperator::Not, Box::new(self.parse_expression()?))
            }
            Token::LParen => self.parse_grouped_expression()?,
            Token::If => self.parse_if_expression()?,

            other_token => {
                return Err(String::from(format!("Unexpected token: {:?}", other_token)));
            }
        };

        Ok(expr)
    }
    fn parse_grouped_expression(&mut self) -> Result<Expression<'l>, String> {
        let mut expr = self.parse_single_expression()?;
        while let Some(peekeed_token) = self.lexer.peek() {
            if InfixOperator::try_from(peekeed_token).is_ok() {
                expr = self.infix_parse(expr, true)?;
            } else {
                if self.lexer.next().unwrap() != Token::RParen {
                    return Err(String::from("Grouped expression not finished"));
                }
                return Ok(expr);
            }
        }

        if self.lexer.next().unwrap() != Token::RParen {
            return Err(String::from("Grouped expression not finished"));
        }
        Ok(expr)
    }
    fn parse_expression(&mut self) -> Result<Expression<'l>, String> {
        let mut expr = self.parse_single_expression()?;
        while let Some(peekeed_token) = self.lexer.peek() {
            if InfixOperator::try_from(peekeed_token).is_ok() {
                expr = self.infix_parse(expr, false)?;
            } else {
                return Ok(expr);
            }
        }

        Ok(expr)
    }
    fn parse_if_expression(&mut self) -> Result<Expression<'l>, String> {
        let Some(Token::LParen) = self.lexer.next() else {
            return Err(String::from(
                "If expression must have an opening parenthesis",
            ));
        };
        let condition = self.parse_expression()?;
        let Some(Token::RParen) = self.lexer.next() else {
            return Err(String::from(
                "If expression: condition must have a closing parenthesis",
            ));
        };
        let Some(Token::LBrace) = self.lexer.next() else {
            return Err(String::from(
                "If expression: concequence must have an opening brace",
            ));
        };

        let concequence = self.parse_statements()?;

        let Some(Token::RBrace) = self.lexer.next() else {
            return Err(String::from(
                "If expression: concequence must have a closing brace",
            ));
        };

        let Some(Token::Else) = self.lexer.next() else {
            return Err(String::from("If expression must have an \"else\" block"));
        };

        let Some(Token::LBrace) = self.lexer.next() else {
            return Err(String::from(
                "If expression: alternative must have an opening brace",
            ));
        };

        let alternative = self.parse_statements()?;

        let Some(Token::RBrace) = self.lexer.next() else {
            return Err(String::from(
                "If expression: alternative must have a closing brace",
            ));
        };

        Ok(Expression::If(
            Box::new(condition),
            Box::new(concequence),
            Box::new(alternative),
        ))
    }
    fn parse_statements(&mut self) -> Result<Vec<Statement<'l>>, String> {
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
                        return Err(String::from("Return statement without the semicolon"));
                    };
                }
                // Encountered the end of block
                Token::RBrace => {
                    break;
                }
                _ => {
                    let expr = self.parse_expression()?;

                    statements.push(Statement::Expression(expr));

                    if let Some(token) = self.lexer.peek() {
                        match token {
                            Token::Semicolon => {
                                self.lexer.next();
                            }
                            Token::RBrace => {}
                            _ => {
                                return Err(String::from("Statement without the semicolon"));
                            }
                        }
                    }
                }
            }
        }

        Ok(statements)
    }
    #[allow(dead_code)]
    fn parse_program(mut self) -> Result<Program<'l>, String> {
        Ok(Program::new(self.parse_statements()?))
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
5 + 4 > 6;
3 > 5 == false;
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
                false,
            )),
            false,
        )),
        Statement::Expression(Expression::Infix(
            InfixOperator::Add,
            Box::new(Expression::Infix(
                InfixOperator::Divide,
                Box::new(Expression::IntLiteral(10)),
                Box::new(Expression::IntLiteral(2)),
                false,
            )),
            Box::new(Expression::IntLiteral(5)),
            false,
        )),
        Statement::Expression(Expression::Infix(
            InfixOperator::LessThan,
            Box::new(Expression::Infix(
                InfixOperator::Subtract,
                Box::new(Expression::IntLiteral(10)),
                Box::new(Expression::IntLiteral(2)),
                false,
            )),
            Box::new(Expression::Infix(
                InfixOperator::Add,
                Box::new(Expression::IntLiteral(7)),
                Box::new(Expression::IntLiteral(3)),
                false,
            )),
            false,
        )),
        // 5 + 4 > 6;
        Statement::Expression(Expression::Infix(
            InfixOperator::GreaterThan,
            Box::new(Expression::Infix(
                InfixOperator::Add,
                Box::new(Expression::IntLiteral(5)),
                Box::new(Expression::IntLiteral(4)),
                false,
            )),
            Box::new(Expression::IntLiteral(6)),
            false,
        )),
        // 3 > 5 == false;
        Statement::Expression(Expression::Infix(
            InfixOperator::Equals,
            Box::new(Expression::Infix(
                InfixOperator::GreaterThan,
                Box::new(Expression::IntLiteral(3)),
                Box::new(Expression::IntLiteral(5)),
                false,
            )),
            Box::new(Expression::Boolean(false)),
            false,
        )),
    ]);

    assert_eq!(parsed_ast, expected_ast);
}

#[test]
fn grouped() {
    let input = "
(5 + 10) / 2;";

    let lexer = Lexer::new(input);
    let parser = Parser::new(lexer);
    let parsed_ast = parser
        .parse_program()
        .expect("Should be parsed successfully");
    let expected_ast = Program::new(vec![Statement::Expression(Expression::Infix(
        InfixOperator::Divide,
        Box::new(Expression::Infix(
            InfixOperator::Add,
            Box::new(Expression::IntLiteral(5)),
            Box::new(Expression::IntLiteral(10)),
            true,
        )),
        Box::new(Expression::IntLiteral(2)),
        false,
    ))]);

    assert_eq!(parsed_ast, expected_ast);
}

#[test]
fn boolean() {
    let input = "
true;
false;
let foobar = true;
let barfoo = false;";

    let lexer = Lexer::new(input);
    let parser = Parser::new(lexer);
    let parsed_ast = parser
        .parse_program()
        .expect("Should be parsed successfully");

    let expected_ast = Program::new(vec![
        Statement::Expression(Expression::Boolean(true)),
        Statement::Expression(Expression::Boolean(false)),
        Statement::Let("foobar", Expression::Boolean(true)),
        Statement::Let("barfoo", Expression::Boolean(false)),
    ]);

    assert_eq!(parsed_ast, expected_ast);
}

#[test]
fn if_expression() {
    let input = "if (x < y) { let a = x - 2; a } else { y }";

    let lexer = Lexer::new(input);

    let lexer = Lexer::new(input);
    let parser = Parser::new(lexer);
    let parsed_ast = parser
        .parse_program()
        .expect("Should be parsed successfully");

    let expected_ast = Program::new(vec![Statement::Expression(Expression::If(
        Box::new(Expression::Infix(
            InfixOperator::LessThan,
            Box::new(Expression::Identifier("x")),
            Box::new(Expression::Identifier("y")),
            false,
        )),
        Box::new(vec![
            Statement::Let(
                "a",
                Expression::Infix(
                    InfixOperator::Subtract,
                    Box::new(Expression::Identifier("x")),
                    Box::new(Expression::IntLiteral(2)),
                    false,
                ),
            ),
            Statement::Expression(Expression::Identifier("a")),
        ]),
        Box::new(vec![Statement::Expression(Expression::Identifier("y"))]),
    ))]);
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
            false,
        )),
        Statement::Expression(Expression::Infix(
            InfixOperator::Subtract,
            Box::new(Expression::IntLiteral(5)),
            Box::new(Expression::IntLiteral(5)),
            false,
        )),
        Statement::Expression(Expression::Infix(
            InfixOperator::Multiply,
            Box::new(Expression::IntLiteral(5)),
            Box::new(Expression::IntLiteral(5)),
            false,
        )),
        Statement::Expression(Expression::Infix(
            InfixOperator::Divide,
            Box::new(Expression::IntLiteral(5)),
            Box::new(Expression::IntLiteral(5)),
            false,
        )),
        Statement::Expression(Expression::Infix(
            InfixOperator::GreaterThan,
            Box::new(Expression::IntLiteral(5)),
            Box::new(Expression::IntLiteral(5)),
            false,
        )),
        Statement::Expression(Expression::Infix(
            InfixOperator::LessThan,
            Box::new(Expression::IntLiteral(5)),
            Box::new(Expression::IntLiteral(5)),
            false,
        )),
        Statement::Expression(Expression::Infix(
            InfixOperator::Equals,
            Box::new(Expression::IntLiteral(5)),
            Box::new(Expression::IntLiteral(5)),
            false,
        )),
        Statement::Expression(Expression::Infix(
            InfixOperator::NotEquals,
            Box::new(Expression::IntLiteral(5)),
            Box::new(Expression::IntLiteral(5)),
            false,
        )),
    ]);
    assert_eq!(parsed_ast, expected_ast);
}
