use crate::runtime::gc::{Trace, Gc, Mark};

pub struct Array {
    pub items: Vec<Value>
}

impl Array {
    pub fn new() -> Self {
        Array {
            items: Vec::new()
        }
    }
}

impl Trace for Array {
    fn trace(&self, mark: Mark) {
        for item in self.items.iter() {
            item.trace(mark);
        }
    }
}

pub enum Value {
    Null,
    Int(i64),
    Float(f64),
    Array(Gc<Array>)
}

impl Trace for Value {
    fn trace(&self, mark: Mark) {
        match self {
            Value::Array(array) => array.trace(mark),
            _ => {}
        }
    }
}