use std::fmt::Display;

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Object<'i> {
    Integer(i32),
    Boolean(bool),
    Function { arguments: Vec<&'i str> },
    Null,
}

impl<'ast> Display for Object<'ast> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::Integer(integer) => write!(f, "{}", integer),
            Object::Boolean(boolean) => write!(f, "{}", boolean),
            Object::Null => write!(f, "null"),
            Object::Function { arguments } => todo!(),
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
