use std::fmt::Display;

use crate::{ast::Statement, evaluation::Environment};

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Object {
    Integer(i32),
    Boolean(bool),
    String(String),
    Function {
        arguments: Vec<String>,
        body: Vec<Statement>,
        captured_env: Environment,
    },
    BuiltInFunction(String),
    Null,
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::Integer(integer) => write!(f, "{}", integer),
            Object::Boolean(boolean) => write!(f, "{}", boolean),
            Object::String(string) => write!(f, "\"{}\"", string),
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
            Object::BuiltInFunction(name) => write!(f, "Builtin function: {}", name),
        }
    }
}
impl Object {
    pub fn is_truthy(&self) -> bool {
        match self {
            Object::Integer(int) => *int > 0,
            Object::Boolean(b) => *b,
            Object::String(string) => string.len() > 0,
            Object::Null => false,
            Object::Function { .. } => false,
            Object::BuiltInFunction(_) => false,
        }
    }
}
