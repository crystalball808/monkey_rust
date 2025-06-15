use std::{collections::HashMap, fmt::Display};

use crate::{
    ast::{Expression, InfixOperator, PrefixOperator, Statement},
    object::Object,
};

#[derive(Debug, Clone, PartialEq)]
pub struct Environment {
    pub store: HashMap<String, Object>,
}
impl PartialOrd for Environment {
    fn partial_cmp(&self, _other: &Self) -> Option<std::cmp::Ordering> {
        None
    }
}
impl Environment {
    pub fn new() -> Self {
        Self {
            store: HashMap::new(),
        }
    }
    pub fn set(&mut self, key: impl Into<String>, value: Object) {
        self.store.insert(key.into(), value);
    }
    pub fn get(&self, key: &str) -> Option<&Object> {
        self.store.get(key)
    }
}

pub fn eval_statements(
    statements: Vec<Statement>,
    env: &mut Environment,
) -> Result<ReturnableObject, Error> {
    let mut result = ReturnableObject(Object::Null, false);

    for statement in statements {
        let res = eval_statement(statement, env)?;
        if res.1 {
            return Ok(res);
        }
        result = res
    }

    Ok(result)
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum Error {
    InfixTypeMismatch(InfixOperator, Object, Object),
    PrefixTypeMismatch(PrefixOperator, Object),
    IdentifierNotFound(String),
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
            Error::IdentifierNotFound(ident) => {
                write!(f, "Identifier not found: {}", ident)
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
                other => Err(Error::PrefixTypeMismatch(PrefixOperator::Negative, other)),
            }
        }
        Expression::Prefix(PrefixOperator::Not, expr) => match eval_expression(*expr, env)?.0 {
            Object::Integer(integer) => Ok(Object::Boolean(integer == 0).into()),
            Object::Boolean(boolean) => Ok(Object::Boolean(!boolean).into()),
            Object::Null => Ok(Object::Boolean(true).into()),
            obj @ Object::Function { .. } => {
                Err(Error::PrefixTypeMismatch(PrefixOperator::Not, obj))
            }
        },
        Expression::Infix(infix_operator, left_expr, right_expr, _) => {
            eval_infix(infix_operator, *left_expr, *right_expr, env).map(Object::into)
        }
        Expression::Identifier(ident) => env
            .get(&ident)
            .map(|obj| obj.clone().into())
            .ok_or(Error::IdentifierNotFound(ident)),
        Expression::If(condition, consequence, alternative) => {
            let condition = eval_expression(*condition, env)?.0;
            if condition.is_truthy() {
                eval_statements(consequence, env)
            } else {
                if alternative.is_some() {
                    eval_statements(alternative.unwrap(), env)
                } else {
                    Ok(Object::Null.into())
                }
            }
        }
        Expression::Func(arguments, body) => Ok(Object::Function {
            arguments,
            body,
            env: Environment::new(),
        }
        .into()),
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
        (_, Object::Function { .. }, _) | (_, _, Object::Function { .. }) => Err(
            Error::InfixTypeMismatch(infix_operator, left_obj, right_obj),
        ),
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
#[cfg_attr(test, derive(PartialEq))]
pub struct ReturnableObject(pub Object, bool);
impl Into<ReturnableObject> for Object {
    fn into(self) -> ReturnableObject {
        ReturnableObject(self, false)
    }
}
fn eval_statement(statement: Statement, env: &mut Environment) -> Result<ReturnableObject, Error> {
    match statement {
        Statement::Let(identifier, expression) => {
            let res = eval_expression(expression, env);
            if let Ok(obj) = res {
                env.set(identifier, obj.0);
                Ok(Object::Null.into())
            } else {
                res
            }
        }
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
    struct Pair {
        input: &'static str,
        output: Object,
    }

    #[test]
    fn integer() {
        let lexer = Lexer::new("10;");
        let parser = Parser::new(lexer);
        let parsed_ast = parser
            .parse_program()
            .expect("Should be parsed successfully");
        let mut environment = Environment {
            store: HashMap::new(),
        };

        let result =
            eval_statements(parsed_ast.statements, &mut environment).expect("Should evaluate");

        assert_eq!(result.0, Object::Integer(10));
    }

    #[test]
    fn let_bindings() {
        let tests = vec![
            Pair {
                input: "let a = 5; a;",
                output: Object::Integer(5),
            },
            Pair {
                input: "let a = 5 * 5; a;",
                output: Object::Integer(25),
            },
            Pair {
                input: "let a = 5; let b = a; b;",
                output: Object::Integer(5),
            },
            Pair {
                input: "let a = 5; let b = a; let c = a + b + 5; c;",
                output: Object::Integer(15),
            },
        ];
        for test in tests {
            let lexer = Lexer::new(test.input);
            let parser = Parser::new(lexer);
            let parsed_ast = parser
                .parse_program()
                .expect("Should be parsed successfully");
            let mut environment = Environment {
                store: HashMap::new(),
            };
            let result =
                eval_statements(parsed_ast.statements, &mut environment).expect("Should evaluate");

            assert_eq!(result.0, test.output, "input: {}", test.input);
        }
    }

    #[test]
    fn identifier_not_found() {
        let lexer = Lexer::new("foobar;");
        let parser = Parser::new(lexer);
        let parsed_ast = parser
            .parse_program()
            .expect("Should be parsed successfully");
        let mut environment = Environment {
            store: HashMap::new(),
        };
        let result = eval_statements(parsed_ast.statements, &mut environment);

        assert_eq!(
            result,
            Result::Err(Error::IdentifierNotFound("foobar".to_owned()))
        );
    }

    #[test]
    fn function_expression() {
        let lexer = Lexer::new("fn(x) { x + 2; }");
        let parser = Parser::new(lexer);
        let parsed_ast = parser
            .parse_program()
            .expect("Should be parsed successfully");
        let mut environment = Environment {
            store: HashMap::new(),
        };
        let result =
            eval_statements(parsed_ast.statements, &mut environment).expect("Should evaluate");

        assert_eq!(
            result.0,
            Object::Function {
                arguments: vec![String::from("x")],
                body: vec![Statement::Expression(Expression::Infix(
                    InfixOperator::Add,
                    Box::new(Expression::Identifier(String::from("x"))),
                    Box::new(Expression::IntLiteral(2)),
                    false
                ))],
                env: Environment::new()
            }
        );
    }

    #[test]
    fn return_statements() {
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
            let mut environment = Environment {
                store: HashMap::new(),
            };
            let result =
                eval_statements(parsed_ast.statements, &mut environment).expect("Should evaluate");

            assert_eq!(result.0, test.output, "input: {}", test.input);
        }
    }
}
