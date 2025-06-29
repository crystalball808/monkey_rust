use std::{cell::RefCell, collections::HashMap, fmt::Display, rc::Rc};

use crate::{
    ast::{Expression, InfixOperator, PrefixOperator, Statement},
    object::Object,
};

pub struct Environment<'ast> {
    pub store: HashMap<&'ast str, Object<'ast>>,
    outer: Option<Rc<RefCell<Environment<'ast>>>>,
}
impl<'ast> Environment<'ast> {
    pub fn new() -> Self {
        Self {
            store: HashMap::new(),
            outer: None,
        }
    }
    pub fn with_outer(outer: Rc<RefCell<Environment<'ast>>>) -> Self {
        Self {
            store: HashMap::new(),
            outer: Some(outer),
        }
    }
    pub fn set(&mut self, key: &'ast str, value: Object<'ast>) {
        self.store.insert(key, value);
    }
    pub fn get(&self, key: &'ast str) -> Option<Object<'ast>> {
        self.store.get(key).cloned()
    }
}

pub fn eval_statements<'ast>(
    statements: Vec<Statement<'ast>>,
    env: Rc<RefCell<Environment<'ast>>>,
) -> Result<ReturnableObject<'ast>, Error<'ast>> {
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
pub enum Error<'ast> {
    InfixTypeMismatch(InfixOperator, Object<'ast>, Object<'ast>),
    PrefixTypeMismatch(PrefixOperator, Object<'ast>),
    IdentifierNotFound(&'ast str),
}
impl Display for Error<'_> {
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
impl std::error::Error for Error<'_> {}

fn eval_expression<'ast>(
    expr: Expression<'ast>,
    env: Rc<RefCell<Environment<'ast>>>,
) -> Result<ReturnableObject<'ast>, Error<'ast>> {
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
                Object::Function { arguments } => todo!(),
            }
        }
        Expression::Prefix(PrefixOperator::Not, expr) => match eval_expression(*expr, env)?.0 {
            Object::Integer(integer) => Ok(Object::Boolean(integer == 0).into()),
            Object::Boolean(boolean) => Ok(Object::Boolean(!boolean).into()),
            Object::Null => Ok(Object::Boolean(true).into()),
            Object::Function { arguments } => todo!(),
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
        Expression::Identifier(ident) => env
            .borrow()
            .get(&ident)
            .map(|obj| ReturnableObject(obj.clone(), false))
            .ok_or(Error::IdentifierNotFound(ident)),
        Expression::If(condition, consequence, alternative) => {
            let condition = { eval_expression(*condition, env.clone())?.0 };
            if condition.is_truthy() {
                eval_statements(
                    consequence,
                    Rc::new(RefCell::new(Environment::with_outer(env))),
                )
            } else {
                if alternative.is_some() {
                    eval_statements(
                        alternative.unwrap(),
                        Rc::new(RefCell::new(Environment::with_outer(env))),
                    )
                } else {
                    Ok(Object::Null.into())
                }
            }
        }
        Expression::Func(vec, vec1) => todo!(),
        Expression::Call(expression, vec) => todo!(),
    }
}

fn eval_infix<'ast>(
    infix_operator: InfixOperator,
    left_expr: Expression<'ast>,
    right_expr: Expression<'ast>,
    env: Rc<RefCell<Environment<'ast>>>,
) -> Result<Object<'ast>, Error<'ast>> {
    let ReturnableObject(left_obj, _) = eval_expression(left_expr, env.clone())?;
    let ReturnableObject(right_obj, _) = eval_expression(right_expr, env.clone())?;
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
pub struct ReturnableObject<'ast>(pub Object<'ast>, bool);
impl<'ast> Into<ReturnableObject<'ast>> for Object<'ast> {
    fn into(self) -> ReturnableObject<'ast> {
        ReturnableObject(self, false)
    }
}
fn eval_statement<'ast>(
    statement: Statement<'ast>,
    env: Rc<RefCell<Environment<'ast>>>,
) -> Result<ReturnableObject<'ast>, Error<'ast>> {
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

        assert_eq!(result, Result::Err(Error::IdentifierNotFound("foobar")));
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
}
