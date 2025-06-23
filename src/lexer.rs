use crate::Token;

fn read_word(input: &str) -> &str {
    let first_nonletter_index = input
        .find(|ch: char| !ch.is_alphabetic() && ch != '_')
        .unwrap_or(1);

    &input[0..first_nonletter_index]
}
fn read_string(input: &str) -> Result<&str, String> {
    let end_i = input[1..]
        .find(|ch| ch == '"')
        .ok_or(String::from("String literal not finished"))?;

    Ok(&input[1..=end_i])
}
fn read_int(input: &str) -> &str {
    let first_nondigit_index = input.find(|ch: char| !ch.is_numeric()).unwrap_or(1);

    &input[0..first_nondigit_index]
}
fn lookup_keyword(word: &str) -> Token {
    match word {
        "let" => Token::Let,
        "fn" => Token::Function,
        "true" => Token::True,
        "false" => Token::False,
        "return" => Token::Return,
        "if" => Token::If,
        "else" => Token::Else,
        identifier => Token::Identifier(identifier),
    }
}

#[derive(Debug)]
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
        let mut chars = self.input.chars();
        let ch = chars.next()?;

        let t = match ch {
            '"' => {
                let str = read_string(self.input).expect("Should read a string");
                self.input = &self.input[(str.len() + 2)..];

                return Some(Token::String(str));
            }
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
            '=' => {
                if let Some('=') = chars.next() {
                    self.input = &self.input[2..];
                    return Some(Token::Equals);
                }

                Token::Assign
            }
            ';' => Token::Semicolon,
            '(' => Token::LParen,
            ')' => Token::RParen,
            '{' => Token::LBrace,
            '}' => Token::RBrace,
            '[' => Token::LBracket,
            ']' => Token::RBracket,
            ',' => Token::Comma,
            '+' => Token::Plus,
            '-' => Token::Minus,
            '!' => {
                if let Some('=') = chars.next() {
                    self.input = &self.input[2..];
                    return Some(Token::NotEquals);
                }

                Token::Bang
            }
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
    let input = "=+-!*/<>
10 == 10;
9 != 10;";

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
        Int(10),
        Equals,
        Int(10),
        Semicolon,
        Int(9),
        NotEquals,
        Int(10),
        Semicolon,
    ];

    let lexer = Lexer::new(input);

    let output: Vec<Token> = lexer.collect();

    assert_eq!(output, expected_output);
}

#[test]
fn keywords() {
    let input = "if (5 < 10) {
    return true;
} else {
    return false;
}
";

    use Token::*;
    let expected_output = vec![
        If,
        LParen,
        Int(5),
        LessThan,
        Int(10),
        RParen,
        LBrace,
        Return,
        True,
        Semicolon,
        RBrace,
        Else,
        LBrace,
        Return,
        False,
        Semicolon,
        RBrace,
    ];
    let lexer = Lexer::new(input);

    let output: Vec<Token> = lexer.collect();

    assert_eq!(output, expected_output);
}

#[test]
fn strings() {
    let input = "let title = \"foo\";";
    use Token::*;
    let expected_result = vec![Let, Identifier("title"), Assign, String("foo"), Semicolon];

    let lexer = Lexer::new(input);

    let output: Vec<Token> = lexer.collect();

    assert_eq!(output, expected_result);
}

#[test]
fn basic_set() {
    let input = "let five = 5;
let name = \"Roman\";
let ten = 10;

let add = fn(x, y) {
  x + y
};
let result = add(five, ten);
[1, 2];
";

    use Token::*;
    let expected_result = vec![
        Let,
        Identifier("five"),
        Assign,
        Int(5),
        Semicolon,
        Let,
        Identifier("name"),
        Assign,
        String("Roman"),
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
        LBracket,
        Int(1),
        Comma,
        Int(2),
        RBracket,
        Semicolon,
    ];

    let lexer = Lexer::new(input);

    let output: Vec<Token> = lexer.collect();

    assert_eq!(output, expected_result);
}
