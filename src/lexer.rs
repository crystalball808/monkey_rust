use crate::Token;

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
        let ch = self.input.chars().next()?;

        let t = match ch {
            '=' => Some(Token::Assign),
            ';' => Some(Token::Semicolon),
            '(' => Some(Token::LParen),
            ')' => Some(Token::RParen),
            '{' => Some(Token::LBrace),
            '}' => Some(Token::RBrace),
            ',' => Some(Token::Comma),
            '+' => Some(Token::Plus),
            _ => Some(Token::Illegal),
        };
        self.input = &self.input[1..];

        t
    }
}

#[test]
fn test_lexer() {
    let input = "=+(){}";

    let expected_result = vec![
        Token::Assign,
        Token::Plus,
        Token::LParen,
        Token::RParen,
        Token::LBrace,
        Token::RBrace,
    ];

    let lexer = Lexer::new(input);

    let output: Vec<Token> = lexer.collect();

    assert_eq!(output, expected_result);
}
