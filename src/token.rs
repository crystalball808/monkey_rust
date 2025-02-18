#[derive(Debug, PartialEq)]
pub enum Token<'a> {
    Illegal,

    // Identifiers + literals
    Identifier(&'a str),
    Int(u32),

    // Operators
    Assign,
    Plus,

    // Delimiters
    Comma,
    Semicolon,

    LParen,
    RParen,
    LBrace,
    RBrace,

    // Keywords
    Function,
    Let,
}
