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

    pub fn new_scope(&mut self) -> Root<Scope> {
        let parent = self.stack.last().cloned();
        self.env.construct(Scope::new(parent))
    }

    pub fn push(&mut self) -> Gc<Scope> {
        let scope = self.new_scope().decay();
        self.stack.push(scope);
        self.top()
    }

    fn with_scope<F>(&mut self, f: F) -> Result<Option<Value>>
    where F: FnOnce(&mut Self) -> Result<Option<Value>> {
        self.push();
        let res = f(self);
        self.pop();
        res
    }

    pub fn read(&self, name: &Ident) -> Result<Value> {
        self.top().root().get(name).ok_or(RuntimeError::UndefinedVariable)
    }

    pub fn write(&self, name: &Ident, value: Value) {
        self.top().root().set(name, value)
    }

    pub fn execute(&mut self, stmt: &Stmt) -> Result<Option<Value>> {
        self.with_scope(|rt| rt.stmt(stmt))
    }

    pub fn stmt(&mut self, stmt: &Stmt) -> Result<Option<Value>> {
        use Stmt::*;

        match stmt {
            Compound(stmts) => self.with_scope(|rt| rt.compound(stmts)),
            If(cond, csq, alt) => self.with_scope(|rt| rt.if_stmt(cond.as_ref(), csq.as_ref(), alt.as_ref().map(|alt| alt.as_ref()))),
            While(cond, csq) => self.with_scope(|rt| rt.while_stmt(cond.as_ref(), csq.as_ref())),
            Return(expr) => self.expr(expr).map(|v| Some(v)),
            Expr(expr) => self.expr(expr).map(|_| None),
            FnDecl(name, params, body) => {
                let mut scope = self.top();
                let func = Function{
                    parent_scope: scope.clone(),
                    params: params.clone(),
                    body: body.clone()
                };
                let func = self.env.construct(func);
                scope.root().set(name, Value::Function(func.decay()));
                Ok(None)
            }
        }
    }

    fn compound(&mut self, stmts: &Vec<Stmt>) -> Result<Option<Value>> {
        for stmt in stmts.iter() {
            if let rv @ Some(_) = self.stmt(stmt)? {
                return Ok(rv);
            }
        }

        Ok(None)
    }

    fn if_stmt(&mut self, cond: &Expr, csq: &Stmt, alt: Option<&Stmt>) -> Result<Option<Value>> {
        if self.expr(cond)?.truthy() {
            self.stmt(csq)
        } else if let Some(alt) = alt {
            self.stmt(alt)
        } else {
            Ok(None)
        }
    }

    fn while_stmt(&mut self, cond: &Expr, csq: &Stmt) -> Result<Option<Value>> {
        while self.expr(cond)?.truthy() {
            if let rv @ Some(_) = self.stmt(csq)? {
                return Ok(rv);
            }
        }

        Ok(None)
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
            Call(callee, args) => {
                let callee = self.expr(callee)?;
                self.call(callee, args)
            },
            LValue(expr) => self.lvalue_as_rvalue(expr),
            Assignment(left, right) => {
                let right = self.expr(right)?;
                self.assignment(left, right)
            }
        }
    }

    pub fn call(&mut self, callee: Value, args: &Vec<Expr>) -> Result<Value> {
        let func = match callee {
            Value::Function(func) => func,
            _ => Err(RuntimeError::NotAFunction)?
        };

        let func = func.root();

        if func.params.len() != args.len() {
            return Err(RuntimeError::InvalidArguments)
        }

        let mut scope = self.new_scope();
        let params = func.params.clone();
        let body = func.body.clone();
        func.unroot();

        for (name, expr) in params.into_iter().zip(args) {
            scope.set(&name, self.expr(expr)?);
        }

        self.stack.push(scope.unroot());
        let res = self.stmt(body.as_ref());
        self.pop();

        match res {
            Ok(None) => Ok(Value::Null),
            Ok(Some(val)) => Ok(val),
            Err(err) => Err(err)
        }
    }

    fn assignment(&mut self, left: &LValueExpr, right: Value) -> Result<Value> {
        use LValueExpr::*;

        match left {
            Ident(name) => self.write(name, right.clone())
        }

        Ok(right)
    }

    fn lvalue_as_rvalue(&mut self, expr: &LValueExpr) -> Result<Value> {
        use LValueExpr::*;

        match expr {
            Ident(name) => self.read(name)
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
    UndefinedVariable,
    InvalidArguments,
    NotAFunction
}

impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            RuntimeError::DivideByZero => write!(f, "Divide by Zero"),
            RuntimeError::Arithmetic => write!(f, "Arithmetic operator on unsupported type(s)"),
            RuntimeError::NotYetImplemented => write!(f, "Not yet implemented"),
            RuntimeError::UndefinedVariable => write!(f, "Undefined variable"),
            RuntimeError::InvalidArguments => write!(f, "Invalid arguments"),
            RuntimeError::NotAFunction => write!(f, "Value is not a function")
        }
    }
}

impl Error for RuntimeError {
    fn description(&self) -> &str {
        match self {
            RuntimeError::DivideByZero => "Runtime error: Divide by Zero",
            RuntimeError::Arithmetic => "Runtime error: Arithmetic",
            RuntimeError::NotYetImplemented => "Runtime error: Not yet implemented",
            RuntimeError::UndefinedVariable => "Runtime error: Undefined variable",
            RuntimeError::InvalidArguments => "Runtime error: Invalid arguments",
            RuntimeError::NotAFunction => "Runtime error: Value is not a function"
        }
    }
}