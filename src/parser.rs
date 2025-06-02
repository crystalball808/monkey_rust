use std::iter::Peekable;

use crate::{
    Lexer, Token,
    ast::{Expression, InfixOperator, PrefixOperator, Program, Statement},
};

pub struct Parser<'l> {
    lexer: Peekable<Lexer<'l>>,
}

impl<'l> Parser<'l> {
    pub fn new(lexer: Lexer<'l>) -> Self {
        Self {
            lexer: lexer.peekable(),
        }
    }
    fn infix_parse(
        &mut self,
        left_expr: Expression<'l>,
        boosted: bool,
    ) -> Result<Expression<'l>, String> {
        let infix_operator = {
            let tkn = self.lexer.next().ok_or("Failed to parse token")?;
            InfixOperator::try_from(&tkn).map_err(|_| "Should be valid infix operator")?
        };

        if let Expression::Infix(left_infix_operator, _, _, false) = &left_expr {
            if left_infix_operator.get_precedence() >= infix_operator.get_precedence() {
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
            Token::Identifier(identifier) => {
                let expr = Expression::Identifier(identifier);
                if let Some(Token::LParen) = self.lexer.peek() {
                    self.parse_call(expr)?
                } else {
                    expr
                }
            }
            Token::Minus => {
                Expression::Prefix(PrefixOperator::Negative, Box::new(self.parse_expression()?))
            }
            Token::Bang => {
                Expression::Prefix(PrefixOperator::Not, Box::new(self.parse_expression()?))
            }
            Token::LParen => self.parse_grouped_expression()?,
            Token::If => self.parse_if_expression()?,
            Token::Function => {
                let expr = self.parse_function_literal()?;
                if let Some(Token::LParen) = self.lexer.peek() {
                    self.parse_call(expr)?
                } else {
                    expr
                }
            }

            other_token => {
                return Err(String::from(format!(
                    "Parse single expession: Unexpected token: {:?}",
                    other_token
                )));
            }
        };

        Ok(expr)
    }
    fn parse_function_literal(&mut self) -> Result<Expression<'l>, String> {
        let mut param_names = Vec::new();

        // parse function parameters
        let Some(Token::LParen) = self.lexer.next() else {
            return Err(String::from("Function literal should have parameters"));
        };

        if let Some(Token::RParen) = self.lexer.peek() {
            self.lexer.next();
        } else {
            while let Some(token) = self.lexer.next() {
                if let Token::Identifier(param_name) = token {
                    param_names.push(param_name)
                } else {
                    return Err(format!(
                        "Invalid function parameter syntax: parameter expected, got {:?}",
                        token
                    ));
                }

                let Some(next_token) = self.lexer.next() else {
                    return Err(String::from(
                        "Invalid function parameter syntax: not finished",
                    ));
                };

                match next_token {
                    Token::Comma => {}
                    Token::RParen => break,
                    other => {
                        return Err(format!(
                            "Invalid function parameter syntax: comma or right parenthesis expected, got {:?}",
                            other
                        ));
                    }
                }
            }
        };

        // parse function body
        let Some(Token::LBrace) = self.lexer.next() else {
            return Err(String::from(
                "Invalid function body syntax: body must have an opening brace",
            ));
        };

        let body = self.parse_statements()?;

        let Some(Token::RBrace) = self.lexer.next() else {
            return Err(String::from(
                "Invalid function body syntax: body must have a closing brace",
            ));
        };

        Ok(Expression::Func(param_names, body))
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
    fn parse_call(&mut self, func_expr: Expression<'l>) -> Result<Expression<'l>, String> {
        assert!(matches!(
            func_expr,
            Expression::Func(_, _) | Expression::Identifier(_)
        ),);

        let Some(Token::LParen) = self.lexer.next() else {
            return Err(String::from("Call expression must have parentheses"));
        };
        let mut arguments = Vec::new();

        loop {
            arguments.push(self.parse_expression()?);
            match self.lexer.peek() {
                Some(Token::RParen) => {
                    self.lexer.next();
                    break;
                }
                Some(Token::Comma) => {
                    self.lexer.next();
                }
                other => {
                    return Err(format!(
                        "Parse call: expected right parenthesis or comma, got {:?}",
                        other
                    ));
                }
            }
        }

        Ok(Expression::Call(Box::new(func_expr), arguments))
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

        let alternative = if let Some(Token::Else) = self.lexer.peek() {
            self.lexer.next();

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
            Some(alternative)
        } else {
            None
        };

        Ok(Expression::If(
            Box::new(condition),
            concequence,
            alternative,
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
                        _ => return Err(String::from("Illegal syntax")),
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
    pub fn parse_program(mut self) -> Result<Program<'l>, String> {
        Ok(Program::new(self.parse_statements()?))
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use Expression::*;
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
            Statement::Let("x", IntLiteral(5)),
            Statement::Let("y", IntLiteral(10)),
            Statement::Let("foobar", IntLiteral(838383)),
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
            Statement::Return(IntLiteral(5)),
            Statement::Return(IntLiteral(993322)),
            Statement::Return(Identifier("foobar")),
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
            Statement::Expression(Identifier("foobar")),
            Statement::Expression(IntLiteral(5)),
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
            Statement::Expression(Prefix(PrefixOperator::Negative, Box::new(IntLiteral(5)))),
            Statement::Expression(Prefix(
                PrefixOperator::Negative,
                Box::new(Identifier("foobar")),
            )),
            Statement::Expression(Prefix(PrefixOperator::Not, Box::new(IntLiteral(10)))),
            Statement::Expression(Prefix(PrefixOperator::Not, Box::new(Identifier("x")))),
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
            Statement::Expression(Infix(
                InfixOperator::Add,
                Box::new(IntLiteral(5)),
                Box::new(Infix(
                    InfixOperator::Divide,
                    Box::new(IntLiteral(10)),
                    Box::new(IntLiteral(2)),
                    false,
                )),
                false,
            )),
            Statement::Expression(Infix(
                InfixOperator::Add,
                Box::new(Infix(
                    InfixOperator::Divide,
                    Box::new(IntLiteral(10)),
                    Box::new(IntLiteral(2)),
                    false,
                )),
                Box::new(IntLiteral(5)),
                false,
            )),
            Statement::Expression(Infix(
                InfixOperator::LessThan,
                Box::new(Infix(
                    InfixOperator::Subtract,
                    Box::new(IntLiteral(10)),
                    Box::new(IntLiteral(2)),
                    false,
                )),
                Box::new(Infix(
                    InfixOperator::Add,
                    Box::new(IntLiteral(7)),
                    Box::new(IntLiteral(3)),
                    false,
                )),
                false,
            )),
            // 5 + 4 > 6;
            Statement::Expression(Infix(
                InfixOperator::GreaterThan,
                Box::new(Infix(
                    InfixOperator::Add,
                    Box::new(IntLiteral(5)),
                    Box::new(IntLiteral(4)),
                    false,
                )),
                Box::new(IntLiteral(6)),
                false,
            )),
            // 3 > 5 == false;
            Statement::Expression(Infix(
                InfixOperator::Equals,
                Box::new(Infix(
                    InfixOperator::GreaterThan,
                    Box::new(IntLiteral(3)),
                    Box::new(IntLiteral(5)),
                    false,
                )),
                Box::new(Boolean(false)),
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
        let expected_ast = Program::new(vec![Statement::Expression(Infix(
            InfixOperator::Divide,
            Box::new(Infix(
                InfixOperator::Add,
                Box::new(IntLiteral(5)),
                Box::new(IntLiteral(10)),
                true,
            )),
            Box::new(IntLiteral(2)),
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
            Statement::Expression(Boolean(true)),
            Statement::Expression(Boolean(false)),
            Statement::Let("foobar", Boolean(true)),
            Statement::Let("barfoo", Boolean(false)),
        ]);

        assert_eq!(parsed_ast, expected_ast);
    }

    #[test]
    fn function_literal() {
        let input = "fn(x, y) { return x + y; }";
        let lexer = Lexer::new(input);
        let parser = Parser::new(lexer);
        let parsed_ast = parser
            .parse_program()
            .expect("Should be parsed successfully");

        let expected_ast = Program::new(vec![Statement::Expression(Func(
            vec!["x", "y"],
            vec![Statement::Return(Infix(
                InfixOperator::Add,
                Box::new(Identifier("x")),
                Box::new(Identifier("y")),
                false,
            ))],
        ))]);

        assert_eq!(parsed_ast, expected_ast);
    }
    #[test]
    fn if_expression() {
        let input = "if (x < y) { let a = x - 2; a } else { y }";

        let lexer = Lexer::new(input);
        let parser = Parser::new(lexer);
        let parsed_ast = parser
            .parse_program()
            .expect("Should be parsed successfully");

        let expected_ast = Program::new(vec![Statement::Expression(If(
            Box::new(Infix(
                InfixOperator::LessThan,
                Box::new(Identifier("x")),
                Box::new(Identifier("y")),
                false,
            )),
            vec![
                Statement::Let(
                    "a",
                    Infix(
                        InfixOperator::Subtract,
                        Box::new(Identifier("x")),
                        Box::new(IntLiteral(2)),
                        false,
                    ),
                ),
                Statement::Expression(Identifier("a")),
            ],
            vec![Statement::Expression(Identifier("y"))].into(),
        ))]);
        assert_eq!(parsed_ast, expected_ast);
    }
    #[test]
    fn call_expression() {
        let input = "add(1, 2 * 3, 4 + 5);";
        let lexer = Lexer::new(input);
        let parser = Parser::new(lexer);
        let parsed_ast = parser
            .parse_program()
            .expect("Should be parsed successfully");

        let expected_ast = Program::new(vec![Statement::Expression(Call(
            Box::new(Identifier("add")),
            vec![
                IntLiteral(1),
                Infix(
                    InfixOperator::Multiply,
                    Box::new(IntLiteral(2)),
                    Box::new(IntLiteral(3)),
                    false,
                ),
                Infix(
                    InfixOperator::Add,
                    Box::new(IntLiteral(4)),
                    Box::new(IntLiteral(5)),
                    false,
                ),
            ],
        ))]);
        assert_eq!(parsed_ast, expected_ast);
    }
    #[test]
    fn infix_call() {
        let input = "a + add(b * c) - d";
        let lexer = Lexer::new(input);
        let parser = Parser::new(lexer);
        let parsed_ast = parser
            .parse_program()
            .expect("Should be parsed successfully");

        let expected_ast = Program::new(vec![Statement::Expression(Infix(
            InfixOperator::Subtract,
            Box::new(Infix(
                InfixOperator::Add,
                Box::new(Identifier("a")),
                Box::new(Call(
                    Box::new(Identifier("add")),
                    vec![Infix(
                        InfixOperator::Multiply,
                        Box::new(Identifier("b")),
                        Box::new(Identifier("c")),
                        false,
                    )],
                )),
                false,
            )),
            Box::new(Identifier("d")),
            false,
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
            Statement::Expression(Infix(
                InfixOperator::Add,
                Box::new(IntLiteral(5)),
                Box::new(IntLiteral(5)),
                false,
            )),
            Statement::Expression(Infix(
                InfixOperator::Subtract,
                Box::new(IntLiteral(5)),
                Box::new(IntLiteral(5)),
                false,
            )),
            Statement::Expression(Infix(
                InfixOperator::Multiply,
                Box::new(IntLiteral(5)),
                Box::new(IntLiteral(5)),
                false,
            )),
            Statement::Expression(Infix(
                InfixOperator::Divide,
                Box::new(IntLiteral(5)),
                Box::new(IntLiteral(5)),
                false,
            )),
            Statement::Expression(Infix(
                InfixOperator::GreaterThan,
                Box::new(IntLiteral(5)),
                Box::new(IntLiteral(5)),
                false,
            )),
            Statement::Expression(Infix(
                InfixOperator::LessThan,
                Box::new(IntLiteral(5)),
                Box::new(IntLiteral(5)),
                false,
            )),
            Statement::Expression(Infix(
                InfixOperator::Equals,
                Box::new(IntLiteral(5)),
                Box::new(IntLiteral(5)),
                false,
            )),
            Statement::Expression(Infix(
                InfixOperator::NotEquals,
                Box::new(IntLiteral(5)),
                Box::new(IntLiteral(5)),
                false,
            )),
        ]);
        assert_eq!(parsed_ast, expected_ast);
    }
}
