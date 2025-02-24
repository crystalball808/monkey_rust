use crate::Token;

pub struct Lexer<'i> {
    input: &'i str,
}

impl<'i> Lexer<'i> {
    pub fn new(input: &'i str) -> Self {
        Self { input }
    }
}

fn read_word<'a>(input: &'a str) -> &'a str {
    let first_nonletter_index = input
        .find(|ch: char| !ch.is_alphabetic())
        .expect("Should have at least one alpabetic char");

    &input[0..first_nonletter_index]
}

impl<'i> Iterator for Lexer<'i> {
    type Item = Token<'i>;

    fn next(&mut self) -> Option<Self::Item> {
        let ch = self.input.chars().next()?;

        let t = match ch {
            letter if letter.is_alphabetic() => {
                let word = read_word(&self.input);

                self.input = &self.input[word.len()..];

                if word == "let" {
                    return Some(Token::Let);
                }
                return Some(Token::Identifier(word));
            }
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
        self.input = &self.input[1..];

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
