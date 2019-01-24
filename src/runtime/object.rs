use crate::runtime::gc::Gc;

pub struct Array {
    pub items: Vec<Gc>
}

pub enum Value {
    Int(i64),
    Float(f64),
    Object(Gc)
}

pub enum Object {
    Int(i64),
    Float(i64),
    Array(Array)
}
