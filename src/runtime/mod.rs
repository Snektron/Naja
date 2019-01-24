pub mod scope;
pub mod object;
pub mod gc;

use std::fmt;
use std::error::Error;
use crate::ast::*;
use crate::runtime::gc::*;

pub type Result<T> = std::result::Result<T, RuntimeError>;

pub struct Runtime {

}

impl Runtime {
    pub fn new() -> Self {
        Runtime {

        }
    }

    pub fn execute(stmt: &Stmt) {

    }
}

#[derive(Debug)]
pub enum RuntimeError {
    DivideByZero
}

impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            RuntimeError::DivideByZero => write!(f, "Divide by Zero"),
        }
    }
}

impl Error for RuntimeError {
    fn description(&self) -> &str {
        match self {
            RuntimeError::DivideByZero => "Runtime error: Divide by Zero"
        }
    }
}