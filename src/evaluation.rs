use std::{collections::HashMap, fmt::Display};

use crate::{
    ast::{Expression, InfixOperator, PrefixOperator, Statement},
    object::Object,
};

struct Environment<'i> {
    store: HashMap<&'i str, Object>,
}

pub fn eval_statements(statements: Vec<Statement>) -> Result<ReturnableObject, Error> {
    let mut result = ReturnableObject(Object::Null, false);
    let mut environment = Environment {
        store: HashMap::new(),
    };

    for statement in statements {
        let res = eval_statement(statement, &mut environment)?;
        if res.1 {
            return Ok(res);
        }
        result = res
    }

    Ok(result)
}

#[derive(Debug)]
pub enum Error {
    InfixTypeMismatch(InfixOperator, Object, Object),
    PrefixTypeMismatch(PrefixOperator, Object),
}
impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::InfixTypeMismatch(op, left, right) => {
                write!(f, "Type mismatch: {} {} {}", left, op, right)
            }
            Error::PrefixTypeMismatch(op, obj) => {
                write!(f, "Type mismatch: {}{}", op, obj)
            }
        }
    }
}
impl std::error::Error for Error {}

fn eval_expression(expr: Expression, env: &mut Environment) -> Result<ReturnableObject, Error> {
    match expr {
        Expression::IntLiteral(integer) => Ok(Object::Integer(integer).into()),
        Expression::Boolean(boolean) => Ok(Object::Boolean(boolean).into()),
        Expression::Prefix(PrefixOperator::Negative, expr) => {
            match eval_expression(*expr, env)?.0 {
                Object::Integer(integer) => Ok(Object::Integer(-integer).into()),
                obj @ Object::Boolean(_) => {
                    Err(Error::PrefixTypeMismatch(PrefixOperator::Negative, obj))
                }
                Object::Null => Err(Error::PrefixTypeMismatch(
                    PrefixOperator::Negative,
                    Object::Null,
                )),
            }
        }
        Expression::Prefix(PrefixOperator::Not, expr) => match eval_expression(*expr, env)?.0 {
            Object::Integer(integer) => Ok(Object::Boolean(integer == 0).into()),
            Object::Boolean(boolean) => Ok(Object::Boolean(!boolean).into()),
            Object::Null => Ok(Object::Boolean(true).into()),
        },
        Expression::Infix(infix_operator, left_expr, right_expr, _) => {
            eval_infix(infix_operator, *left_expr, *right_expr, env).map(Object::into)
        }
        Expression::Identifier(ident) => todo!(),
        Expression::If(condition, consequence, alternative) => {
            let condition = eval_expression(*condition, env)?.0;
            if condition.is_truthy() {
                eval_statements(consequence)
            } else {
                if alternative.is_some() {
                    eval_statements(alternative.unwrap())
                } else {
                    Ok(Object::Null.into())
                }
            }
        }
        Expression::Func(vec, vec1) => todo!(),
        Expression::Call(expression, vec) => todo!(),
    }
}

fn eval_infix(
    infix_operator: InfixOperator,
    left_expr: Expression,
    right_expr: Expression,
    env: &mut Environment,
) -> Result<Object, Error> {
    let ReturnableObject(left_obj, _) = eval_expression(left_expr, env)?;
    let ReturnableObject(right_obj, _) = eval_expression(right_expr, env)?;
    match (&infix_operator, &left_obj, &right_obj) {
        (InfixOperator::Equals, left_obj, right_obj) => Ok(Object::Boolean(left_obj == right_obj)),
        (InfixOperator::NotEquals, left_obj, right_obj) => {
            Ok(Object::Boolean(left_obj != right_obj))
        }
        (_, Object::Null, _) | (_, _, Object::Null) => Err(Error::InfixTypeMismatch(
            infix_operator,
            left_obj,
            right_obj,
        )),
        (_, Object::Boolean(_), _) | (_, _, Object::Boolean(_)) => Err(Error::InfixTypeMismatch(
            infix_operator,
            left_obj,
            right_obj,
        )),
        (InfixOperator::Add, Object::Integer(left_int), Object::Integer(right_int)) => {
            Ok(Object::Integer(left_int + right_int))
        }
        (InfixOperator::Subtract, Object::Integer(left_int), Object::Integer(right_int)) => {
            Ok(Object::Integer(left_int - right_int))
        }
        (InfixOperator::Multiply, Object::Integer(left_int), Object::Integer(right_int)) => {
            Ok(Object::Integer(left_int * right_int))
        }
        (InfixOperator::Divide, Object::Integer(left_int), Object::Integer(right_int)) => {
            Ok(Object::Integer(left_int / right_int))
        }
        (InfixOperator::LessThan, Object::Integer(left_int), Object::Integer(right_int)) => {
            Ok(Object::Boolean(left_int < right_int))
        }
        (InfixOperator::GreaterThan, Object::Integer(left_int), Object::Integer(right_int)) => {
            Ok(Object::Boolean(left_int > right_int))
        }
    }
}

#[derive(Debug)]
pub struct ReturnableObject(pub Object, bool);
impl Into<ReturnableObject> for Object {
    fn into(self) -> ReturnableObject {
        ReturnableObject(self, false)
    }
}
fn eval_statement(statement: Statement, env: &mut Environment) -> Result<ReturnableObject, Error> {
    match statement {
        Statement::Let(_, expression) => todo!(),
        Statement::Return(expr) => eval_expression(expr, env).map(|mut r| {
            r.1 = true;
            r
        }),
        Statement::Expression(expr) => eval_expression(expr, env),
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::{Lexer, parser::Parser};

    #[test]
    fn integer() {
        let lexer = Lexer::new("10;");
        let parser = Parser::new(lexer);
        let parsed_ast = parser
            .parse_program()
            .expect("Should be parsed successfully");
        let result = eval_statements(parsed_ast.statements).expect("Should evaluate");

        assert_eq!(result.0, Object::Integer(10));
    }

    #[test]
    fn return_statements() {
        struct Pair {
            input: &'static str,
            output: Object,
        }
        let tests = vec![
            Pair {
                input: "return 10;",
                output: Object::Integer(10),
            },
            Pair {
                input: "return 10; 9;",
                output: Object::Integer(10),
            },
            Pair {
                input: "return 2 * 5; 9;",
                output: Object::Integer(10),
            },
            Pair {
                input: r"
                if (10 > 1) {
                    if (10 > 1) {
                        return 10;
                    };
                    return 1;
                };
                ",
                output: Object::Integer(10),
            },
        ];

        for test in tests {
            let lexer = Lexer::new(test.input);
            let parser = Parser::new(lexer);
            let parsed_ast = parser
                .parse_program()
                .expect("Should be parsed successfully");
            let result = eval_statements(parsed_ast.statements).expect("Should evaluate");

            assert_eq!(result.0, test.output, "input: {}", test.input);
        }
    }
}
