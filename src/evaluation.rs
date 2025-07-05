use std::{cell::RefCell, collections::HashMap, fmt::Display, rc::Rc};

use crate::{
    ast::{Expression, InfixOperator, PrefixOperator, Statement},
    object::Object,
};

#[derive(Debug, PartialEq)]
pub struct Environment<'ast> {
    pub store: HashMap<&'ast str, Object<'ast>>,
    outer: Option<Rc<RefCell<Environment<'ast>>>>,
}
impl Clone for Environment<'_> {
    fn clone(&self) -> Self {
        Self {
            store: self.store.clone(),
            outer: self
                .outer
                .clone()
                .map(|o| Rc::new(RefCell::new(o.borrow().clone()))),
        }
    }
}
impl<'ast> Default for Environment<'ast> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'ast> Environment<'ast> {
    pub fn new() -> Self {
        Self {
            store: HashMap::new(),
            outer: None,
        }
    }
    pub fn log(&self) {
        dbg!(self);
    }
    pub fn with_outer(outer: Rc<RefCell<Environment<'ast>>>) -> Self {
        Self {
            store: HashMap::new(),
            outer: Some(outer),
        }
    }
    pub fn add_outer(&mut self, outer: Rc<RefCell<Environment<'ast>>>) {
        self.outer = Some(outer);
    }
    pub fn set(&mut self, key: &'ast str, value: Object<'ast>) {
        self.store.insert(key, value);
    }
    pub fn get(&self, key: &str) -> Option<Object<'ast>> {
        if let Some(val) = self.store.get(key) {
            Some(val.clone())
        } else if let Some(outer) = self.outer.clone() {
            outer.borrow().get(key)
        } else {
            None
        }
    }
}

pub fn eval_statements<'ast>(
    statements: Vec<Statement<'ast>>,
    env: Rc<RefCell<Environment<'ast>>>,
) -> Result<ReturnableObject<'ast>, Error> {
    let mut result = ReturnableObject(Object::Null, false);

    for statement in statements {
        let res = eval_statement(statement, env.clone())?;
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
    InfixTypeMismatch(InfixOperator, String, String),
    PrefixTypeMismatch(PrefixOperator, String),
    IdentifierNotFound(String),
    NotCallable(String),
    ArgumentCountMismatch(String),
    TypeMismatch(String),
    NotIndexable,
    OutOfBounds,
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
            Error::NotIndexable => {
                write!(f, "Not indexable")
            }
            Error::OutOfBounds => {
                write!(f, "Out of bounds")
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

fn eval_expression<'ast>(
    expr: Expression<'ast>,
    env: Rc<RefCell<Environment<'ast>>>,
) -> Result<ReturnableObject<'ast>, Error> {
    match expr {
        Expression::IntLiteral(integer) => Ok(Object::Integer(integer).into()),
        Expression::Boolean(boolean) => Ok(Object::Boolean(boolean).into()),
        Expression::Prefix(PrefixOperator::Negative, expr) => {
            match eval_expression(*expr, env)?.0 {
                Object::Integer(integer) => Ok(Object::Integer(-integer).into()),
                obj @ Object::Boolean(_) => Err(Error::PrefixTypeMismatch(
                    PrefixOperator::Negative,
                    obj.to_string(),
                )),
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
        // Expression::Identifier(ident) => {
        //     let foo = env.get(ident);
        //     let foo = foo.map(|obj| ReturnableObject(obj.clone(), false)).unwrap();
        //
        // Ok(ReturnableObject(Object::Null, false))
        // Ok(foo)

        // foo.ok_or(Error::IdentifierNotFound(ident))
        // Err(Error::IdentifierNotFound(ident))
        // }
        Expression::Identifier(ident) => {
            let env_borrow = env.borrow();
            if let Some(obj) = env_borrow.get(ident) {
                Ok(ReturnableObject(obj.clone(), false))
            } else {
                Err(Error::IdentifierNotFound(ident.to_string()))
            }
        }
        Expression::If(condition, consequence, alternative) => {
            let condition = { eval_expression(*condition, env.clone())?.0 };
            if condition.is_truthy() {
                eval_statements(
                    consequence,
                    Rc::new(RefCell::new(Environment::with_outer(env))),
                )
            } else if alternative.is_some() {
                eval_statements(
                    alternative.unwrap(),
                    Rc::new(RefCell::new(Environment::with_outer(env))),
                )
            } else {
                Ok(Object::Null.into())
            }
        }
        Expression::Func(arguments, body) => Ok(Object::Function {
            arguments,
            body,
            captured_env: env.clone(),
        }
        .into()),
        Expression::Call(expr, passed_values) => {
            let obj = eval_expression(*expr, env.clone())?.0;
            match obj {
                Object::Function {
                    arguments,
                    body,
                    captured_env,
                } => {
                    // Create a new environment for the function call that extends the captured environment
                    let function_env = Rc::new(RefCell::new(Environment::with_outer(captured_env.clone())));
                    // Connect the captured environment to the current environment for recursive calls
                    captured_env.borrow_mut().add_outer(env.clone());
                    
                    if arguments.len() != passed_values.len() {
                        return Err(Error::ArgumentCountMismatch(arguments.join(",")));
                    }
                    for (arg_name, expr) in arguments.into_iter().zip(passed_values.into_iter()) {
                        let obj = eval_expression(expr, env.clone())?.0;
                        function_env.borrow_mut().set(arg_name, obj)
                    }

                    eval_statements(body, function_env)
                }
                not_callable_obj => Err(Error::NotCallable(not_callable_obj.to_string())),
            }
        }
    }
}

fn eval_infix<'ast>(
    infix_operator: InfixOperator,
    left_expr: Expression<'ast>,
    right_expr: Expression<'ast>,
    env: Rc<RefCell<Environment<'ast>>>,
) -> Result<Object<'ast>, Error> {
    let ReturnableObject(left_obj, _) = eval_expression(left_expr, env.clone())?;
    let ReturnableObject(right_obj, _) = eval_expression(right_expr, env.clone())?;
    match (&infix_operator, &left_obj, &right_obj) {
        (InfixOperator::Equals, left_obj, right_obj) => Ok(Object::Boolean(left_obj == right_obj)),
        (InfixOperator::NotEquals, left_obj, right_obj) => {
            Ok(Object::Boolean(left_obj != right_obj))
        }
        (_, Object::Function { .. }, _) | (_, _, Object::Function { .. }) => Err(
            Error::InfixTypeMismatch(infix_operator, left_obj.to_string(), right_obj.to_string()),
        ),
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
pub struct ReturnableObject<'ast>(pub Object<'ast>, bool);
impl<'ast> From<Object<'ast>> for ReturnableObject<'ast> {
    fn from(val: Object<'ast>) -> Self {
        ReturnableObject(val, false)
    }
}
fn eval_statement<'ast>(
    statement: Statement<'ast>,
    env: Rc<RefCell<Environment<'ast>>>,
) -> Result<ReturnableObject<'ast>, Error> {
    match statement {
        Statement::Let(identifier, expression) => {
            let res = eval_expression(expression, env.clone());
            if let Ok(obj) = res {
                env.borrow_mut().set(identifier, obj.0);
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
        output: Object<'static>,
    }

    #[test]
    fn integer() {
        let lexer = Lexer::new("10;");
        let parser = Parser::new(lexer);
        let parsed_ast = parser
            .parse_program()
            .expect("Should be parsed successfully");
        let environment = Rc::new(RefCell::new(Environment::new()));

        let result = eval_statements(parsed_ast.statements, environment).expect("Should evaluate");

        assert_eq!(result.0, Object::Integer(10));
    }

    #[test]
    fn functions() {
        let input = "
let func = fn(x) {
    x + 2
};
func(1)";
        let lexer = Lexer::new(input);
        let parser = Parser::new(lexer);
        let parsed_ast = parser
            .parse_program()
            .expect("Should be parsed successfully");
        let environment = Rc::new(RefCell::new(Environment::new()));

        let result = eval_statements(parsed_ast.statements, environment).expect("Should evaluate");

        assert_eq!(result.0, Object::Integer(3));
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
            let environment = Rc::new(RefCell::new(Environment::new()));
            let result =
                eval_statements(parsed_ast.statements, environment).expect("Should evaluate");

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
        let environment = Rc::new(RefCell::new(Environment::new()));
        let result = eval_statements(parsed_ast.statements, environment);

        assert_eq!(
            result,
            Result::Err(Error::IdentifierNotFound("foobar".to_owned()))
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
            let environment = Rc::new(RefCell::new(Environment::new()));
            let result =
                eval_statements(parsed_ast.statements, environment).expect("Should evaluate");

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
        let environment = Rc::new(RefCell::new(Environment::new()));

        let result = eval_statements(parsed_ast.statements, environment).expect("Should evaluate");

        assert_eq!(result.0, Object::Integer(5));
    }

    #[test]
    fn recursion() {
        let input = "
let fib = fn(x) { 
  if (x < 2) {
      return x;
  } else {
      return fib(x - 2) + fib(x - 1);
  }
};

fib(5)
";
        let lexer = Lexer::new(input);
        let parser = Parser::new(lexer);
        let parsed_ast = parser
            .parse_program()
            .expect("Should be parsed successfully");
        let environment = Rc::new(RefCell::new(Environment::new()));
        let result = eval_statements(parsed_ast.statements, environment).expect("Should evaluate");

        assert_eq!(result.0, Object::Integer(5));
    }
}
