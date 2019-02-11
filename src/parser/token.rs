#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Dot,
    Comma,
    ParenOpen,
    ParenClose,
    BraceOpen,
    BraceClose,

    // Operators
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    EqEq,
    Eq,
    Pipe,

    // Keywords
    Fn,
    If,
    Else,
    While,
    Return,
    True,
    False,

    // Literals
    Null,
    Ident(String),
    Integer(String),
    Float(String)
}