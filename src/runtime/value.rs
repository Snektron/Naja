use std::convert::From;
use std::collections::HashMap;
use std::fmt;
use crate::ast::{Literal, Ident, Stmt};
use crate::runtime::gc::{Trace, Gc, Mark};

pub struct Scope {
    parent: Option<Gc<Scope>>,
    bindings: HashMap<Ident, Value>
}

impl Scope {
    pub fn new(parent: Option<Gc<Scope>>) -> Self {
        Scope {
            parent: parent,
            bindings: HashMap::new()
        }
    }

    pub fn get(&self, name: &Ident) -> Option<Value> {
        if let Some(value) = self.bindings.get(name) {
            Some(value.clone())
        } else if let Some(parent) = self.parent.as_ref() {
            parent.root().get(name)
        } else {
            None
        }
    }

    fn set_r(&mut self, name: &Ident, value: &mut Option<Value>) {
        if let Some(binding) = self.bindings.get_mut(name) {
            *binding = value.take().unwrap()
        } else if let Some(parent) = self.parent.as_mut() {
            parent.root().set_r(name, value)
        }
    }

    pub fn set(&mut self, name: &Ident, value: Value) {
        let mut value = Some(value);
        self.set_r(name, &mut value);
        if let Some(value) = value {
            // no previous binding, create a new one
            self.set_local(name, value);
        }
    }

    pub fn set_local(&mut self, name: &Ident, value: Value) {
        self.bindings.insert(name.clone(), value);
    }
}

impl Trace for Scope {
    fn trace(&self, mark: Mark) {
        self.parent.trace(mark);
        for item in self.bindings.values() {
            item.trace(mark);
        }
    }
}

pub type Array = Vec<Value>;

pub struct Function {
    pub parent_scope: Gc<Scope>,
    pub params: Vec<Ident>,
    pub body: Box<Stmt>
}

impl Trace for Function {
    fn trace(&self, mark: Mark) {
        self.parent_scope.trace(mark);
    }
}

#[derive(Clone, Debug)]
pub enum Value {
    Null,
    Integer(i64),
    Float(f64),
    Bool(bool),
    Array(Gc<Array>),
    Scope(Gc<Scope>),
    Function(Gc<Function>)
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Value::*;

        match self {
            Null => write!(f, "<Null>"),
            Integer(x) => write!(f, "{}", x),
            Float(x) => write!(f, "{}", x),
            Bool(x) => write!(f, "{}", x),
            Array(arr) => write!(f, "<Array@{:?}>", arr),
            Scope(scope) => write!(f, "<Scope@{:?}>", scope),
            Function(func) => write!(f, "<Function@{:?}>", func)
        }
    }
}

impl Value {
    pub fn truthy(&self) -> bool {
        use Value::*;

        match self {
            Null => false,
            Integer(x) => *x != 0,
            Float(x) => *x != 0.0,
            Bool(x) => *x,
            _ => false
        }
    }
}

impl Trace for Value {
    fn trace(&self, mark: Mark) {
        match self {
            Value::Array(array) => array.trace(mark),
            _ => {}
        }
    }
}

impl From<Literal> for Value {
    fn from(literal: Literal) -> Value {
        match literal {
            Literal::Null => Value::Null,
            Literal::Integer(val) => val.into(),
            Literal::Float(val) => val.into(),
            Literal::Bool(val) => val.into()
        }
    }
}

impl From<i64> for Value {
    fn from(val: i64) -> Value {
        Value::Integer(val)
    }
}

impl From<f64> for Value {
    fn from(val: f64) -> Value {
        Value::Float(val)
    }
}

impl From<bool> for Value {
    fn from(val: bool) -> Value {
        Value::Bool(val)
    }
}