use crate::{Token, token};

pub struct Lexer<'i> {
    input: &'i str,
}

impl<'i> Lexer<'i> {
    pub fn new(input: &'i str) -> Self {
        Self { input }
    }
}

impl<'i> Iterator for Lexer<'i> {
    type Item = Token<'i>;

    fn next(&mut self) -> Option<Self::Item> {
        let mut chars = self.input.chars();
        let ch = chars.next()?;
        self.input = chars.as_str();

        let t = match ch {
            '=' => Token::Assign,
            ';' => Token::Semicolon,
            '(' => Token::LParen,
            ')' => Token::RParen,
            '{' => Token::LBrace,
            '}' => Token::RBrace,
            ',' => Token::Comma,
            '+' => Token::Plus,
            _ => Token::Illegal,
        };

        Some(t)
    }
}

#[test]
fn test_lexer() {
    let input = "let five = 5;
let ten = 10;

let add = fn(x, y) {
  x + y
};
let result = add(five, ten);
";

    use Token::*;
    let expected_result = vec![
        Let,
        Identifier("five"),
        Assign,
        Int(5),
        Semicolon,
        Let,
        Identifier("ten"),
        Assign,
        Int(10),
        Semicolon,
        Let,
        Identifier("add"),
        Assign,
        Function,
        LParen,
        Identifier("x"),
        Comma,
        Identifier("y"),
        RParen,
        LBrace,
        Identifier("x"),
        Plus,
        Identifier("y"),
        RBrace,
        Semicolon,
        Let,
        Identifier("result"),
        Assign,
        Identifier("add"),
        LParen,
        Identifier("five"),
        Comma,
        Identifier("ten"),
        LParen,
        Semicolon,
    ];

    let lexer = Lexer::new(input);

    let output: Vec<Token> = lexer.collect();

    assert_eq!(output, expected_result);
}
