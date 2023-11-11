use crate::evaluator::{EvaluatorError, EvaluatorResult};
use crate::object::Object;
use std::{cell::RefCell, collections::HashMap, rc::Rc};
#[derive(Debug, Clone)]
pub struct Environment {
    store: HashMap<String, Object>,
    outer: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new() -> Environment {
        Environment {
            store: HashMap::new(),
            outer: None,
        }
    }

    pub fn new_enclosed_environment(outer: Rc<RefCell<Environment>>) -> Self {
        Environment {
            store: HashMap::new(),
            outer: Some(outer),
        }
    }

    pub fn get(&self, name: &str) -> EvaluatorResult<Object> {
        match self.store.get(name) {
            Some(val) => Ok(val.clone()),
            None => match &self.outer {
                Some(env) => env.borrow().get(name),
                None => Err(EvaluatorError::new(format!(
                    "identifier not found: {}",
                    name
                ))),
            },
        }
    }

    pub fn set(&mut self, name: String, val: &Object) -> Option<Object> {
        self.store.insert(name, val.clone())
    }
}
