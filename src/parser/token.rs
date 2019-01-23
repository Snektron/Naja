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

    // Keywords
    Fn,
    If,
    Else,
    While,
    Return,

    // Literals
    Ident(String),
    Integer(String),
    Float(String)
}