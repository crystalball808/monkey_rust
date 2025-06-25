use std::{collections::HashMap, fmt::Display};

use crate::{
    ast::{Expression, InfixOperator, PrefixOperator, Statement},
    object::Object,
};

#[derive(Debug, Clone, PartialEq)]
pub struct Environment {
    pub store: HashMap<String, Object>,
    outer: Option<Box<Environment>>,
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
            outer: None,
        }
    }
    pub fn with_outer(outer: Environment) -> Self {
        Self {
            store: HashMap::new(),
            outer: Some(Box::new(outer)),
        }
    }
    pub fn add_outer(&mut self, outer: Environment) {
        self.outer = Some(Box::new(outer))
    }
    pub fn set(&'_ mut self, key: impl Into<String>, value: Object) {
        self.store.insert(key.into(), value);
    }
    pub fn get(&self, key: &str) -> Option<&Object> {
        let maybe_val = self.store.get(key);
        if maybe_val.is_none() && self.outer.is_some() {
            let outer = self.outer.as_ref().unwrap();
            outer.get(key)
        } else {
            maybe_val
        }
    }
}

pub fn eval_statements(
    statements: Vec<Statement>,
    env: &mut Environment,
) -> Result<ReturnableObject, Error> {
    let mut result = ReturnableObject(Object::Null, false);

    for statement in statements {
        result = eval_statement(statement, env)?;
        if result.1 {
            return Ok(result);
        }
    }

    Ok(result)
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum Error {
    InfixTypeMismatch(InfixOperator, String, String),
    PrefixTypeMismatch(PrefixOperator, String),
    IdentifierNotFound(String),
    NotCallable(String),
    ArgumentCountMismatch(String),
    TypeMismatch(String),
}
impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::TypeMismatch(obj) => {
                write!(f, "Type mismatch: {}", obj)
            }
            Error::InfixTypeMismatch(op, left, right) => {
                write!(f, "Type mismatch: {} {} {}", left, op, right)
            }
            Error::PrefixTypeMismatch(op, obj) => {
                write!(f, "Type mismatch: {}{}", op, obj)
            }
            Error::IdentifierNotFound(ident) => {
                write!(f, "Identifier not found: {}", ident)
            }
            Error::NotCallable(obj) => {
                write!(f, "Not callable: {}", obj)
            }
            Error::ArgumentCountMismatch(func_name) => {
                write!(
                    f,
                    "Argument count mismatch for function with arguments \"{}\"",
                    func_name
                )
            }
        }
    }
}
impl std::error::Error for Error {}

mod builtin {
    use crate::{ast::Expression, object::Object};

    use super::{Environment, Error, eval_expression};

    pub fn lookup_name(name: &str) -> Option<Object> {
        match name {
            "len" => Some(Object::BuiltInFunction(name.to_owned())),
            _ => None,
        }
    }

    pub fn eval_builtin(
        name: &str,
        mut arguments: impl Iterator<Item = Expression>,
        env: &Environment,
    ) -> Result<Object, Error> {
        match name {
            "len" => {
                let arg = arguments
                    .next()
                    .ok_or(Error::ArgumentCountMismatch("string".to_owned()))?;
                if arguments.next().is_some() {
                    Err(Error::ArgumentCountMismatch("string".to_owned()))
                } else {
                    eval_expression(arg, &env).and_then(|ret_obj| match ret_obj.0 {
                        Object::String(string) => Ok(Object::Integer(string.len() as i32)),
                        Object::Array(array) => Ok(Object::Integer(array.len() as i32)),
                        other => Err(Error::TypeMismatch(other.to_string())),
                    })
                }
            }
            other => Err(Error::NotCallable(other.to_owned())),
        }
    }
}

fn eval_expression(expr: Expression, env: &Environment) -> Result<ReturnableObject, Error> {
    match expr {
        Expression::IntLiteral(integer) => Ok(Object::Integer(integer).into()),
        Expression::StringLiteral(string) => Ok(Object::String(string).into()),
        Expression::ArrayLiteral(exprs) => {
            let mut objs = Vec::with_capacity(exprs.len());
            for expr in exprs {
                let obj = eval_expression(expr, env)?;
                objs.push(obj.0);
            }

            let obj = Object::Array(objs);
            Ok(ReturnableObject(obj, false))
        }
        Expression::Boolean(boolean) => Ok(Object::Boolean(boolean).into()),
        Expression::Prefix(PrefixOperator::Negative, expr) => {
            match eval_expression(*expr, env)?.0 {
                Object::Integer(integer) => Ok(Object::Integer(-integer).into()),
                other => Err(Error::PrefixTypeMismatch(
                    PrefixOperator::Negative,
                    other.to_string(),
                )),
            }
        }
        Expression::Prefix(PrefixOperator::Not, expr) => match eval_expression(*expr, env)?.0 {
            Object::Integer(integer) => Ok(Object::Boolean(integer == 0).into()),
            Object::Boolean(boolean) => Ok(Object::Boolean(!boolean).into()),
            Object::Null => Ok(Object::Boolean(true).into()),
            other => Err(Error::PrefixTypeMismatch(
                PrefixOperator::Not,
                other.to_string(),
            )),
        },
        Expression::Infix(infix_operator, left_expr, right_expr, _) => {
            eval_infix(infix_operator, *left_expr, *right_expr, env).map(Object::into)
        }
        Expression::Identifier(ident) => builtin::lookup_name(&ident)
            .or(env.get(&ident).cloned())
            .map(|obj| ReturnableObject::from(obj))
            .ok_or(Error::IdentifierNotFound(ident)),
        Expression::If(condition, consequence, alternative) => {
            let condition = eval_expression(*condition, env)?.0;
            if condition.is_truthy() {
                eval_statements(consequence, &mut Environment::with_outer(env.clone()))
            } else {
                if alternative.is_some() {
                    eval_statements(
                        alternative.unwrap(),
                        &mut Environment::with_outer(env.clone()),
                    )
                } else {
                    Ok(Object::Null.into())
                }
            }
        }
        Expression::Func(arguments, body) => Ok(Object::Function {
            arguments,
            body,
            captured_env: env.clone(),
        }
        .into()),
        Expression::Call(expr, passed_values) => {
            let obj = eval_expression(*expr, env)?.0;
            match obj {
                Object::Function {
                    arguments,
                    body,
                    mut captured_env,
                } => {
                    captured_env.add_outer(env.clone());

                    if arguments.len() != passed_values.len() {
                        return Err(Error::ArgumentCountMismatch(arguments.join(",")));
                    }
                    for (arg_name, expr) in arguments.into_iter().zip(passed_values.into_iter()) {
                        captured_env.set(arg_name, eval_expression(expr, &env)?.0)
                    }

                    eval_statements(body, &mut captured_env)
                }
                Object::BuiltInFunction(name) => {
                    builtin::eval_builtin(&name, passed_values.into_iter(), env)
                        .map(ReturnableObject::from)
                }
                not_callable_obj => Err(Error::NotCallable(not_callable_obj.to_string())),
            }
        }
    }
}

fn eval_infix(
    infix_operator: InfixOperator,
    left_expr: Expression,
    right_expr: Expression,
    env: &Environment,
) -> Result<Object, Error> {
    let ReturnableObject(left_obj, _) = eval_expression(left_expr, env)?;
    let ReturnableObject(right_obj, _) = eval_expression(right_expr, env)?;
    match (&infix_operator, &left_obj, &right_obj) {
        (InfixOperator::Equals, left_obj, right_obj) => Ok(Object::Boolean(left_obj == right_obj)),
        (InfixOperator::NotEquals, left_obj, right_obj) => {
            Ok(Object::Boolean(left_obj != right_obj))
        }
        (_, Object::Function { .. }, _) | (_, _, Object::Function { .. }) => Err(
            Error::InfixTypeMismatch(infix_operator, left_obj.to_string(), right_obj.to_string()),
        ),
        (_, Object::BuiltInFunction { .. }, _) | (_, _, Object::BuiltInFunction { .. }) => Err(
            Error::InfixTypeMismatch(infix_operator, left_obj.to_string(), right_obj.to_string()),
        ),
        (_, Object::Array(_), _) | (_, _, Object::Array(_)) => Err(Error::InfixTypeMismatch(
            infix_operator,
            left_obj.to_string(),
            right_obj.to_string(),
        )),
        (_, Object::Null, _) | (_, _, Object::Null) => Err(Error::InfixTypeMismatch(
            infix_operator,
            left_obj.to_string(),
            right_obj.to_string(),
        )),
        (_, Object::Boolean(_), _) | (_, _, Object::Boolean(_)) => Err(Error::InfixTypeMismatch(
            infix_operator,
            left_obj.to_string(),
            right_obj.to_string(),
        )),
        (InfixOperator::Add, Object::String(left_str), Object::String(right_string)) => {
            Ok(Object::String(left_str.to_owned() + right_string))
        }
        (_, Object::String(_), _) | (_, _, Object::String(_)) => Err(Error::InfixTypeMismatch(
            infix_operator,
            left_obj.to_string(),
            right_obj.to_string(),
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
impl From<Object> for ReturnableObject {
    fn from(value: Object) -> Self {
        Self(value, false)
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
        let mut environment = Environment::new();

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
            let mut environment = Environment::new();
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
        let mut environment = Environment::new();
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
        let mut environment = Environment::new();
        let result =
            eval_statements(parsed_ast.statements, &mut environment).expect("Should evaluate");

        assert_eq!(
            result.0,
            Object::Function {
                captured_env: Environment::new(),
                arguments: vec![String::from("x")],
                body: vec![Statement::Expression(Expression::Infix(
                    InfixOperator::Add,
                    Box::new(Expression::Identifier(String::from("x"))),
                    Box::new(Expression::IntLiteral(2)),
                    false
                ))],
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
            let mut environment = Environment::new();
            let result =
                eval_statements(parsed_ast.statements, &mut environment).expect("Should evaluate");

            assert_eq!(result.0, test.output, "input: {}", test.input);
        }
    }

    #[test]
    fn currying() {
        let input = "
let add = fn(x) {
    fn (y) {
        x + y
    }
};

let add_two = add(2);

add_two(3)
";
        let lexer = Lexer::new(input);
        let parser = Parser::new(lexer);
        let parsed_ast = parser
            .parse_program()
            .expect("Should be parsed successfully");
        let mut environment = Environment::new();
        let result =
            eval_statements(parsed_ast.statements, &mut environment).expect("Should evaluate");

        assert_eq!(result.0, Object::Integer(5));
    }

    #[test]
    fn recursion() {
        let input = "
let fib = fn(x) { 
  if (x < 2) {
      return x;
  } else {
      return fc(x - 2) + fc(x - 1);
  }
};

fc(5)
";
        let lexer = Lexer::new(input);
        let parser = Parser::new(lexer);
        let parsed_ast = parser
            .parse_program()
            .expect("Should be parsed successfully");
        let mut environment = Environment::new();
        let result =
            eval_statements(parsed_ast.statements, &mut environment).expect("Should evaluate");

        assert_eq!(result.0, Object::Integer(5));
    }
}
