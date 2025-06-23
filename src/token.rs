#[derive(Debug, PartialEq)]
pub enum Token<'a> {
    Illegal,

    // Identifiers + literals
    Identifier(&'a str),
    Int(i32),
    String(&'a str),

    // Operators
    Assign,
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,

    LessThan,
    GreaterThan,

    Equals,
    NotEquals,

    // Delimiters
    Comma,
    Semicolon,

    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,

    // Keywords
    Function,
    Let,
    If,
    Else,
    Return,
    False,
    True,
}
