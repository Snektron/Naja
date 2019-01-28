pub mod lexer;
pub mod token;

use std::iter::Peekable;
use std::error::Error;
use std::fmt;
use crate::parser::token::Token;
use crate::parser::lexer::{Lexer, LexError};
use crate::ast::*;

macro_rules! unexpected {
    ($tok:expr) => (Err(ParseError::UnexpectedToken($tok)))
}

// This is a macro because we can now expect tokens with values, `expect!(self, Token::Literal(_))`
macro_rules! expect {
    ($parser:expr, $tok:pat) => {{
        match $parser.next() {
            Ok(val @ $tok) => Ok(val),
            Ok(tok) => unexpected!(tok),
            err => err
        }
    }}
}

macro_rules! binary_expr_method {
    {$parser:expr, $next:ident $(, $tok:pat => $kind:expr)+} => {{
        let mut lhs = $parser.$next()?;
        loop {
            match $parser.peek_opt()? {
                $(
                    Some($tok) => {
                        $parser.next()?;
                        let rhs = $parser.$next().map(Box::new)?;
                        lhs = Expr::BinOp($kind, Box::new(lhs), rhs);
                    }
                )+
                _ => return Ok(lhs)
            }
        }
    }}
}

pub type Result<T> = std::result::Result<T, ParseError>;

pub struct Parser<'i> {
    lexer: Peekable<Lexer<'i>>,
}

impl<'i> Parser<'i> {
    pub fn new(input: &'i str) -> Self {
        Parser {
            lexer: Lexer::new(input).peekable()
        }
    }

    fn next(&mut self) -> Result<Token> {
        self.lexer
            .next()
            .map(|result| result.map_err(ParseError::LexError))
            .unwrap_or(Err(ParseError::UnexpectedEndOfInput))
    }

    fn peek_opt(&mut self) -> Result<Option<&Token>> {
        match self.lexer.peek() {
            Some(Err(err)) => Err(ParseError::LexError(err.clone())),
            Some(Ok(tok)) => Ok(Some(tok)),
            None => Ok(None)
        }  
    }

    fn peek(&mut self) -> Result<&Token> {
        match self.lexer.peek() {
            Some(Err(err)) => Err(ParseError::LexError(err.clone())),
            Some(Ok(tok)) => Ok(tok),
            None => Err(ParseError::UnexpectedEndOfInput)
        }
    }

    fn at_eof(&mut self) -> bool {
        self.lexer.peek().is_none()
    }

    pub fn program(&mut self) -> Result<Stmt> {
        let mut stmts = Vec::new();
        while !self.at_eof() {
            stmts.push(self.stmt()?);
        }

        Ok(Stmt::Compound(stmts))
    }

    fn stmt(&mut self) -> Result<Stmt> {
        match self.peek()? {
            // Compound statement
            Token::BraceOpen => self.compound_stmt(),
            Token::If => self.if_stmt(),
            Token::While => self.while_stmt(),
            Token::Return => self.return_stmt(),
            Token::Fn => self.fn_stmt(),
            _ => self.expr().map(Box::new).map(Stmt::Expr)   
        }
    }

    fn compound_stmt(&mut self) -> Result<Stmt> {
        expect!(self, Token::BraceOpen)?;
        let mut stmts = Vec::new();
        while *self.peek()? != Token::BraceClose {
            stmts.push(self.stmt()?);
        }

        self.next()?;

        match stmts.len() {
            1 => Ok(stmts.pop().unwrap()),
            _ => Ok(Stmt::Compound(stmts))
        }
    }

    fn if_stmt(&mut self) -> Result<Stmt> {
        expect!(self, Token::If)?;
        let cond = self.expr().map(Box::new)?;
        let body = self.compound_stmt().map(Box::new)?;

        if self.at_eof() || *self.peek()? != Token::Else {
            return Ok(Stmt::If(cond, body, None));
        }
        
        self.next()?;

        let alt = if *self.peek()? == Token::If {
            self.if_stmt().map(Box::new)?
        } else {
            self.compound_stmt().map(Box::new)?
        };

        Ok(Stmt::If(cond, body, Some(alt)))
    }

    fn while_stmt(&mut self) -> Result<Stmt> {
        expect!(self, Token::While)?;
        let cond = self.expr().map(Box::new)?;
        let body = self.compound_stmt().map(Box::new)?;
        Ok(Stmt::While(cond, body))
    }

    fn return_stmt(&mut self) -> Result<Stmt> {
        expect!(self, Token::Return)?;

        let expr = self.expr().map(Box::new)?;
        Ok(Stmt::Return(expr))
    }

    fn fn_stmt(&mut self) -> Result<Stmt> {
        expect!(self, Token::Fn)?;

        let ident = self.ident()?;

        expect!(self, Token::ParenOpen)?;
        let params = self.list_until(Token::ParenClose, Self::ident)?;

        let body = match self.peek()? {
            Token::BraceOpen => self.compound_stmt(),
            Token::Eq => {
                self.next()?;
                self.expr().map(Box::new).map(Stmt::Return)
            },
            tok => unexpected!(tok.clone())
        }?;

        Ok(Stmt::FnDecl(ident, params, Box::new(body)))
    }

    fn expr(&mut self) -> Result<Expr> {
        self.assignment()
    }

    fn assignment(&mut self) -> Result<Expr> {
        let lhs = self.comparison()?;

        if self.at_eof() || *self.peek()? != Token::Eq {
            return Ok(lhs);
        }

        self.next()?;

        if let Expr::LValue(lhs) = lhs {
            let rhs = self.assignment().map(Box::new)?;
            Ok(Expr::Assignment(lhs, rhs))
        } else {
            Err(ParseError::ExpectedLValue)
        }
    }

    fn comparison(&mut self) -> Result<Expr> {
        binary_expr_method! {
            self, sum,
            Token::EqEq => BinOpKind::Equals
        }
    }

    fn sum(&mut self) -> Result<Expr> {
        binary_expr_method! {
            self, factor,
            Token::Plus => BinOpKind::Add,
            Token::Minus => BinOpKind::Sub
        }
    }

    fn factor(&mut self) -> Result<Expr> {
        binary_expr_method! {
            self, unary,
            Token::Star => BinOpKind::Mul,
            Token::Slash => BinOpKind::Div,
            Token::Percent => BinOpKind::Mod
        }
    }

    fn unary(&mut self) -> Result<Expr> {
        macro_rules! unary_expr {
            ($kind:expr) => {{
                self.next()?;
                self.unary()
                    .map(Box::new)
                    .map(|expr| Expr::UnOp($kind, expr))
            }}
        }

        match self.peek()? {
            Token::Minus => unary_expr!(UnOpKind::Neg),
            _ => self.postfix()
        }
    }

    fn postfix(&mut self) -> Result<Expr> {
        let mut expr = self.atom()?;

        loop {
            match self.peek_opt()? {
                Some(Token::ParenOpen) => {
                    self.next()?;
                    let args = self.list_until(Token::ParenClose, Self::expr)?;
                    expr = Expr::Call(Box::new(expr), args);
                },
                _ => return Ok(expr)
            }
        }
    }

    fn list_until<F, R>(&mut self, end: Token, mut f: F) -> Result<Vec<R>>
    where F: FnMut(&mut Self) -> Result<R> {
        let mut list = Vec::new();

        if *self.peek()? == end {
            return self.next().map(|_| list);
        }

        list.push(f(self)?);

        while *self.peek()? != end  {
            expect!(self, Token::Comma)?;
            list.push(f(self)?);
        }

        self.next().map(|_| list)
    }

    fn atom(&mut self) -> Result<Expr> {
        match self.next()? {
            Token::Null => Ok(Expr::Literal(Literal::Null)),
            Token::Integer(text) => Ok(Expr::Literal(Literal::Integer(text.parse::<i64>().expect("int parse")))),
            Token::Float(text) => Ok(Expr::Literal(Literal::Float(text.parse::<f64>().expect("float parse")))),
            Token::ParenOpen => {
                let expr = self.expr()?;
                expect!(self, Token::ParenClose)?;
                Ok(expr)
            },
            Token::Ident(text) => Ok(Expr::LValue(LValueExpr::Ident(text))),
            tok => unexpected!(tok)
        }
    }

    fn ident(&mut self) -> Result<Ident> {
        match self.next()? {
            Token::Ident(ident) => Ok(ident),
            tok => unexpected!(tok)
        }
    }
}

#[derive(Debug)]
pub enum ParseError {
    UnexpectedToken(Token),
    UnexpectedEndOfInput,
    ExpectedLValue,
    LexError(LexError)
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ParseError::UnexpectedToken(tok) => write!(f, "Unexpected token '{:?}'", tok), // TODO: Implement Display for Token
            ParseError::UnexpectedEndOfInput => write!(f, "Unexpected end of input"),
            ParseError::ExpectedLValue => write!(f, "Expected L-Value expression"),
            ParseError::LexError(err) => err.fmt(f)
        }
    }
}

impl Error for ParseError {
    fn description(&self) -> &str {
        match self {
            ParseError::UnexpectedToken(_) => "Parse error: Unexpected token",
            ParseError::UnexpectedEndOfInput => "Parse error: Unexpected end of input",
            ParseError::ExpectedLValue => "Parse error: Expected L-Value expression",
            ParseError::LexError(err) => err.description()
        }
    }
}
