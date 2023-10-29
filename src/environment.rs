use crate::object::Object;
use std::{cell::RefCell, collections::HashMap, rc::Rc};

#[derive(Debug, Clone)]
pub struct Environment {
    store: HashMap<String, Object>,
    outer: Option<RefCell<Rc<Environment>>>,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            store: HashMap::new(),
            outer: None,
        }
    }

    pub fn new_enclosed_environment(outer: Environment) -> Self {
        Environment {
            store: HashMap::new(),
            outer: Some(RefCell::new(Rc::new(outer))),
        }
    }

    pub fn get(&self, name: &str) -> Option<Object> {
        match self.store.get(name) {
            Some(obj) => Some(obj.clone()),
            None => match &self.outer {
                Some(env) => env.borrow().get(name),
                None => None,
            },
        }
    }

    pub fn set(&mut self, name: String, val: &Object) -> Option<Object> {
        self.store.insert(name, val.clone())
    }
}
