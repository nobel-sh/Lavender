use std::cell::RefCell;
use std::fmt::{Debug, Display};
use std::rc::Rc;

use crate::ast::*;
use crate::environment::Environment;

#[derive(Debug, Clone)]
pub struct FunctionObject {
    pub parameters: Vec<String>,
    pub body: Statement,
    pub env: Rc<RefCell<Environment>>,
}

impl Display for FunctionObject {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let params: Vec<String> = self.parameters.iter().map(|p| p.to_string()).collect();
        write!(f, "fn({}) {{\n{}\n}}", params.join(", "), self.body)
    }
}

#[derive(Debug, Clone)]
pub enum Literal {
    Integer(i64),
    Boolean(bool),
    String(String),
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::Integer(i) => write!(f, "{}", i),
            Literal::Boolean(b) => write!(f, "{}", b),
            Literal::String(s) => write!(f, "{}", s),
        }
    }
}

#[derive(Clone)]
pub enum Object {
    Literal(Literal),
    Function(FunctionObject),
    Return(Box<Object>),
    Null,
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::Literal(lit) => write!(f, "{}", lit),
            Object::Function(func) => write!(f, "{}", func),
            Object::Return(obj) => {
                write!(f, "return {}", obj)
            }
            Object::Null => write!(f, "null"),
        }
    }
}

impl Debug for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::Literal(lit) => write!(f, "{}", lit),
            Object::Function(func) => write!(f, "{}", func),
            Object::Return(obj) => {
                write!(f, "return {}", obj)
            }
            Object::Null => write!(f, "null"),
        }
    }
}
