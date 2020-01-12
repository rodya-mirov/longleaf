/// Module for handling namespaces (in a VM)
/// Very simple for now.
use std::collections::HashMap;

use super::PrimitiveValue;

type Lookup = HashMap<String, PrimitiveValue>;

pub struct Namespace {
    call_stack: Vec<Lookup>,
}

impl Namespace {
    pub fn new() -> Self {
        Namespace {
            call_stack: vec![Lookup::new()],
        }
    }

    pub fn start_call(&mut self) {
        self.call_stack.push(Lookup::new());
    }

    pub fn end_call(&mut self) {
        self.call_stack
            .pop()
            .expect("Should have something on the stack to pop off");
    }

    pub fn define_variable(&mut self, name: String, val: PrimitiveValue) -> Option<PrimitiveValue> {
        self.call_stack.last_mut().unwrap().insert(name, val)
    }

    pub fn lookup_variable(&self, name: &str) -> Option<&PrimitiveValue> {
        for i in (0..self.call_stack.len()).rev() {
            let stored = self.call_stack.get(i).unwrap().get(name);
            if stored.is_some() {
                return stored;
            }
        }
        None
    }
}
