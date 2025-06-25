use std::fmt::Display;

use crate::{ast::Statement, evaluation::Environment};

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Object {
    Integer(i32),
    Boolean(bool),
    String(String),
    Array(Vec<Object>),
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
            Object::Array(array) => {
                let mut formatted = String::new();
                for (idx, obj) in array.iter().enumerate() {
                    let last_idx = array.len() - 1;
                    formatted.push_str(&obj.to_string());
                    if idx != last_idx {
                        formatted.push_str(", ")
                    }
                }
                write!(f, "[{}]", formatted)
            }
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
            Object::String(string) => !string.is_empty(),
            _ => false,
        }
    }
}
