use std::fmt::Display;

use crate::{ast::Statement, evaluation::Environment};

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Object {
    Integer(i32),
    Boolean(bool),
    Function {
        arguments: Vec<String>,
        body: Vec<Statement>,
        env: Environment,
    },
    Null,
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::Integer(integer) => write!(f, "{}", integer),
            Object::Boolean(boolean) => write!(f, "{}", boolean),
            Object::Null => write!(f, "null"),
            Object::Function {
                arguments,
                body,
                env: _,
            } => write!(
                f,
                "function({} argumesnts) {{ {} statements }}",
                arguments.len(),
                body.len()
            ),
        }
    }
}
impl Object {
    pub fn is_truthy(&self) -> bool {
        match self {
            Object::Integer(int) => *int > 0,
            Object::Boolean(b) => *b,
            Object::Null => false,
            Object::Function { .. } => false,
        }
    }
}
