pub mod gc;
pub mod value;

use std::fmt;
use std::error::Error;
use crate::ast::*;
use crate::runtime::gc::*;
use crate::runtime::value::*;

pub type Result<T> = std::result::Result<T, RuntimeError>;

pub struct Runtime {
    env: Environment,
    stack: Vec<Gc<Scope>>
}

impl Runtime {
    pub fn new() -> Self {
        let mut rt = Runtime {
            env: Environment::new(),
            stack: Vec::new()
        };

        rt.push();
        rt
    }

    pub fn top(&self) -> Gc<Scope> {
        self.stack.last().unwrap().clone()
    }

    pub fn pop(&mut self) -> Gc<Scope> {
        if self.stack.len() == 1 {
            panic!("Popping all scopes makes runtime invalid");
        }

        self.stack.pop().unwrap()
    }

    pub fn push(&mut self) -> Gc<Scope> {
        self.stack.push(self.env.construct(Scope::new()).decay());
        self.top()
    }

    pub fn execute(&mut self, stmt: &Stmt) -> Result<Value> {
        self.push();
        let result = self.stmt(stmt);
        self.pop();
        result
    }

    pub fn stmt(&mut self, stmt: &Stmt) -> Result<Value> {
        use Stmt::*;

        match stmt {
            Return(expr) => self.expr(expr),
            _ => Err(RuntimeError::NotYetImplemented)
        }
    }

    fn expr(&mut self, expr: &Expr) -> Result<Value> {
        use Expr::*;

        match expr {
            BinOp(kind, left, right) => {
                let left = self.expr(left)?;
                let right = self.expr(right)?;
                self.binop(kind.clone(), left, right)
            },
            UnOp(kind, value) => {
                let value = self.expr(value)?;
                self.unop(kind.clone(), value)
            },
            Literal(literal) => Ok(Value::from(literal.clone())),
            Call(..) => Err(RuntimeError::NotYetImplemented),
            LValue(expr) => self.lvalue_as_rvalue(expr),
            Assignment(left, right) => {
                let right = self.expr(right)?;
                self.assignment(left, right)
            }
        }
    }

    fn assignment(&mut self, left: &LValueExpr, right: Value) -> Result<Value> {
        use LValueExpr::*;

        match left {
            Ident(name) => self.top().root().set(name, right.clone())
        }

        Ok(right)
    }

    fn lvalue_as_rvalue(&mut self, expr: &LValueExpr) -> Result<Value> {
        use LValueExpr::*;

        match expr {
            Ident(name) => self.top().root().get(name).ok_or(RuntimeError::UndefinedVariable)
        }
    }

    fn binop(&mut self, kind: BinOpKind, left: Value, right: Value) -> Result<Value> {
        use BinOpKind::*;
        use Value::*;

        macro_rules! simple_op {
            ($op:expr) => {{
                match (left, right) {
                    (Integer(x), Integer(y)) => Ok($op(x, y).into()),
                    (Float(x), Integer(y)) => Ok($op(x, y as f64).into()),
                    (Integer(x), Float(y)) => Ok($op(x as f64, y).into()),
                    (Float(x), Float(y)) => Ok($op(x, y).into()),
                    _ => Err(RuntimeError::Arithmetic)
                }
            }}
        }

        macro_rules! simple_ref_op {
            ($op:expr) => {{
                match (left, right) {
                    (Integer(x), Integer(y)) => Ok($op(&x, &y).into()),
                    (Float(x), Integer(y)) => Ok($op(&x, &(y as f64)).into()),
                    (Integer(x), Float(y)) => Ok($op(&(x as f64), &y).into()),
                    (Float(x), Float(y)) => Ok($op(&x, &y).into()),
                    _ => Err(RuntimeError::Arithmetic)
                }
            }}
        }

        macro_rules! div_by_zero_op {
            ($op:expr) => {{
                match &right {
                    Integer(y) if *y == 0 => Err(RuntimeError::DivideByZero),
                    Float(y) if *y == 0.0 => Err(RuntimeError::DivideByZero),
                    _ => simple_op!($op)
                }
            }}
        }

        match kind {
            Add => simple_op!(std::ops::Add::add),
            Sub => simple_op!(std::ops::Sub::sub),
            Mul => simple_op!(std::ops::Mul::mul),
            Div => div_by_zero_op!(std::ops::Div::div),
            Mod => div_by_zero_op!(std::ops::Rem::rem),
            Equals => simple_ref_op!(std::cmp::PartialEq::eq),
            _ => Err(RuntimeError::Arithmetic)
        }
    }

    fn unop(&mut self, kind: UnOpKind, value: Value) -> Result<Value> {
        use UnOpKind::*;
        use Value::*;

        macro_rules! simple_op {
            ($trait:path, $op:ident) => {{
                use $trait;

                match value {
                    Integer(x) => Ok(Integer(i64::$op(x))),
                    Float(x) => Ok(Float(f64::$op(x))),
                    _ => Err(RuntimeError::Arithmetic)
                }
            }}
        }

        match kind {
            Neg => simple_op!(std::ops::Neg, neg)
        }
    }

    pub fn gc(&mut self) {
        self.env.gc(&self.stack);
    }
}

#[derive(Debug)]
pub enum RuntimeError {
    DivideByZero,
    Arithmetic,
    NotYetImplemented,
    UndefinedVariable
}

impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            RuntimeError::DivideByZero => write!(f, "Divide by Zero"),
            RuntimeError::Arithmetic => write!(f, "Arithmetic operator on unsupported type(s)"),
            RuntimeError::NotYetImplemented => write!(f, "Not yet implemented"),
            RuntimeError::UndefinedVariable => write!(f, "Undefined variable")
        }
    }
}

impl Error for RuntimeError {
    fn description(&self) -> &str {
        match self {
            RuntimeError::DivideByZero => "Runtime error: Divide by Zero",
            RuntimeError::Arithmetic => "Runtime error: Arithmetic",
            RuntimeError::NotYetImplemented => "Runtime error: Not yet implemented",
            RuntimeError::UndefinedVariable => "Runtime error: Undefined variable"
        }
    }
}