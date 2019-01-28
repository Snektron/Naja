use crate::runtime::gc::{Trace, Gc};

pub struct Array {
    items: Vec<Value>
}

impl Trace for Array {
    fn trace(&self, mark: usize) {
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
    fn trace(&self, mark: usize) {
        match self {
            Value::Array(array) => array.trace(mark),
            _ => {}
        }
    }
}