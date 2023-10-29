use std::fmt::{Debug, Display};

use crate::ast::*;
use crate::environment::Environment;

#[derive(Clone)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    String(String),
    Function(FunctionObject),
    Return(Box<Object>),
    Null,
}

impl Object {
    pub fn inspect(&self) -> String {
        match self {
            Object::Integer(i) => i.to_string(),
            Object::Boolean(b) => b.to_string(),
            Object::String(s) => s.to_string(),
            Object::Function(f) => f.to_string(),
            Object::Return(o) => o.inspect(),
            Object::Null => String::from("null"),
        }
    }
}

impl Debug for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::Integer(i) => write!(f, "{}", i),
            Object::Boolean(b) => write!(f, "{}", b),
            Object::String(s) => write!(f, "{}", s),
            Object::Function(func) => write!(f, "{}", func),
            Object::Return(o) => write!(f, "{}", o.inspect()),
            Object::Null => write!(f, "null"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct FunctionObject {
    pub parameters: Vec<String>,
    pub body: Statement,
    pub env: Environment,
}

impl Display for FunctionObject {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let params: Vec<String> = self.parameters.iter().map(|p| p.to_string()).collect();
        write!(f, "fn({}) {{\n{}\n}}", params.join(", "), self.body)
    }
}
