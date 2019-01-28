use std::error::Error;
use std::str::Chars;
use std::iter::{Iterator, Peekable};
use std::collections::HashMap;
use std::fmt;
use crate::parser::token::Token;

lazy_static! {
    static ref KEYWORDS: HashMap<&'static str, Token> = {
        let mut map = HashMap::new();
        map.insert("fn", Token::Fn);
        map.insert("if", Token::If);
        map.insert("else", Token::Else);
        map.insert("while", Token::While);
        map.insert("return", Token::Return);
        map.insert("null", Token::Null);
        map
    };
}

pub type LexResult = Result<Token, LexError>;

pub struct Lexer<'i> {
    input: Peekable<Chars<'i>>
}

impl<'i> Lexer<'i> {
    pub fn new(input: &'i str) -> Self {
        Lexer {
            input: input.chars().peekable()
        }
    }

    fn peek(&mut self) -> Option<char> {
        self.input.peek().cloned()
    }

    fn consume(&mut self) -> Option<char> {
        self.input.next()
    }

    fn discard_whitespace(&mut self) {
        while let Some(c) = self.peek() {
            if c.is_whitespace() {
                self.consume();
            } else {
                return;
            }
        }
    }

    fn word(&mut self) -> Token {
        let mut word = self.consume().unwrap().to_string();

        while let Some(c) = self.peek() {
            if c.is_alphanumeric() || !c.is_ascii() || c == '_' || c == '?' {
                word.push(self.consume().unwrap());
            } else {
                break;
            }
        }

        KEYWORDS
            .get(word.as_str())
            .cloned()
            .unwrap_or(Token::Ident(word))
    }

    fn number(&mut self) -> Token {
        let mut number = self.consume().unwrap().to_string();

        while let Some(c) = self.peek() {
            if c.is_digit(10) {
                number.push(self.consume().unwrap());
            } else {
                break;
            }
        }

        if self.peek() != Some('.') {
            return Token::Integer(number);
        }

        self.consume();
        number.push('.');

        while let Some(c) = self.peek() {
            if c.is_digit(10) {
                number.push(self.consume().unwrap());
            } else {
                break;
            }
        }

        Token::Float(number)
    }

    fn next_inner(&mut self, c: char) -> LexResult {
        macro_rules! token {
            ($tok:path) => {{
                self.consume();
                Ok($tok)
            }}
        }

        macro_rules! double_token {
            {_ => $default:path $(,$id:pat => $tok:path)+} => {{
                self.consume();
                match self.peek() {
                    $(
                        Some($id) => token!($tok),
                    )+
                    None => Ok($default),
                    Some(_) => Ok($default) 
                }
            }}
        }

        match c {
            '.' => token!(Token::Dot),
            ',' => token!(Token::Comma),
            '(' => token!(Token::ParenOpen),
            ')' => token!(Token::ParenClose),
            '{' => token!(Token::BraceOpen),
            '}' => token!(Token::BraceClose),
            '+' => token!(Token::Plus),
            '-' => token!(Token::Minus),
            '*' => token!(Token::Star),
            '/' => token!(Token::Slash),
            '%' => token!(Token::Percent),
            '=' => double_token!{
                _ => Token::Eq,
                '=' => Token::EqEq
            },
            c if c.is_alphabetic() || !c.is_ascii() || c == '_' => Ok(self.word()),
            c if c.is_digit(10) => Ok(self.number()),
            c => Err(LexError::UnexpectedChar(c))
        }
    }
}

impl<'i> Iterator for Lexer<'i> {
    type Item = LexResult;

    fn next(&mut self) -> Option<Self::Item> {
        self.discard_whitespace();
        self.peek().map(|c| self.next_inner(c))
    }
}

#[derive(Debug, Clone)]
pub enum LexError {
    UnexpectedChar(char),
}

impl fmt::Display for LexError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            LexError::UnexpectedChar(c) => write!(f, "Unexpected character '{}'", c),
        }
    }
}

impl Error for LexError {
    fn description(&self) -> &str {
        match self {
            LexError::UnexpectedChar(_) => "Lexer error: Unexpected character"
        }
    }
}