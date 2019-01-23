pub type Ident = String;

#[derive(Debug, Clone)]
pub enum Literal {
    Integer(i64),
    Float(f64)
}

#[derive(Debug, Clone)]
pub enum BinOpKind {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Equals
}

#[derive(Debug, Clone)]
pub enum UnOpKind {
    Neg
}

#[derive(Debug, Clone)]
pub enum Expr {
    BinOp(BinOpKind, Box<Expr>, Box<Expr>),
    UnOp(UnOpKind, Box<Expr>),
    Literal(Literal),
    Call(Box<Expr>, Vec<Expr>),
    LValue(LValueExpr),
    Assignment(LValueExpr, Box<Expr>)
}

#[derive(Debug, Clone)]
pub enum LValueExpr {
    Ident(Ident)
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Compound(Vec<Stmt>),
    If(Box<Expr>, Box<Stmt>, Option<Box<Stmt>>),
    While(Box<Expr>, Box<Stmt>),
    Return(Box<Expr>),
    Expr(Box<Expr>),
}