use crate::Token;

fn read_word(input: &str) -> &str {
    let first_nonletter_index = input
        .find(|ch: char| !ch.is_alphabetic() || ch == '_')
        .expect("Should have at least one alphabetic char");

    &input[0..first_nonletter_index]
}
fn read_int(input: &str) -> &str {
    let first_nondigit_index = input
        .find(|ch: char| !ch.is_numeric())
        .expect("Should have at least one alphabetic char");

    &input[0..first_nondigit_index]
}
fn lookup_keyword(word: &str) -> Token {
    match word {
        "let" => Token::Let,
        "fn" => Token::Function,
        "true" => Token::True,
        "false" => Token::False,
        "return" => Token::Return,
        identifier => Token::Identifier(identifier),
    }
}

pub struct Lexer<'i> {
    input: &'i str,
}

impl<'i> Lexer<'i> {
    pub fn new(input: &'i str) -> Self {
        Self { input }
    }
    fn skip_whitespace(&mut self) {
        self.input = self.input.trim_start()
    }
}

impl<'i> Iterator for Lexer<'i> {
    type Item = Token<'i>;

    fn next(&mut self) -> Option<Self::Item> {
        self.skip_whitespace();
        let ch = self.input.chars().next()?;

        let t = match ch {
            letter if letter.is_alphabetic() || letter == '_' => {
                let word = read_word(&self.input);

                self.input = &self.input[word.len()..];

                return Some(lookup_keyword(word));
            }
            digit if digit.is_numeric() => {
                let number = read_int(&self.input);

                self.input = &self.input[number.len()..];

                return Some(Token::Int(
                    number.parse().expect("Should be parsed successfully"),
                ));
            }
            '=' => Token::Assign,
            ';' => Token::Semicolon,
            '(' => Token::LParen,
            ')' => Token::RParen,
            '{' => Token::LBrace,
            '}' => Token::RBrace,
            ',' => Token::Comma,
            '+' => Token::Plus,
            '-' => Token::Minus,
            '!' => Token::Bang,
            '*' => Token::Asterisk,
            '/' => Token::Slash,
            '<' => Token::LessThan,
            '>' => Token::GreaterThan,
            _ => Token::Illegal,
        };
        self.input = &self.input[1..];

        Some(t)
    }
}

#[test]
fn operators() {
    let input = "=+-!*/<>";

    use Token::*;
    let expected_output = vec![
        Assign,
        Plus,
        Minus,
        Bang,
        Asterisk,
        Slash,
        LessThan,
        GreaterThan,
    ];

    let lexer = Lexer::new(input);

    let output: Vec<Token> = lexer.collect();

    assert_eq!(output, expected_output);
}

#[test]
fn basic_set() {
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
        RParen,
        Semicolon,
    ];

    let lexer = Lexer::new(input);

    let output: Vec<Token> = lexer.collect();

    assert_eq!(output, expected_result);
}
