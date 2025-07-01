use std::{cell::RefCell, fmt::Display, rc::Rc};

use crate::{ast::Statement, evaluation::Environment};

#[derive(Debug, Clone, PartialEq)]
pub enum Object<'i> {
    Integer(i32),
    Boolean(bool),
    Function {
        arguments: Vec<&'i str>,
        body: Vec<Statement<'i>>,
        captured_env: Rc<RefCell<Environment<'i>>>,
    },
    Null,
}

impl<'ast> Display for Object<'ast> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::Integer(integer) => write!(f, "{}", integer),
            Object::Boolean(boolean) => write!(f, "{}", boolean),
            Object::Null => write!(f, "null"),
            Object::Function {
                arguments,
                body,
                captured_env: _,
            } => write!(
                f,
                "function({} argumesnts) {{ {} statements }}",
                arguments.len(),
                body.len()
            ),
        }
    }
}
impl Object<'_> {
    pub fn is_truthy(&self) -> bool {
        match self {
            Object::Integer(int) => *int > 0,
            Object::Boolean(b) => *b,
            Object::Null => false,
            Object::Function { .. } => false,
        }
    }
}
