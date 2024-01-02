use std::cmp::min;

use crate::lexer::LiteralKind::{DIGIT, EQ, LETTER, OTHER, WHITESPACE};

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum TokenKind {
    Eof,

    Bang,
    Assign,

    Comma,
    Semicolon,

    Plus,
    Minus,
    Asterisk,
    Slash,

    Lt,
    Gt,

    Lparen,
    Rparen,

    Lbrace,
    Rbrace,

    Equals,
    Differs,

    Function,
    Let,
    True,
    False,
    If,
    Else,
    Return,

    Ident { name: String },
    Int { value: i32 },

    Illegal { value: String },
    Blank { value: String },
}

#[derive(Eq, PartialEq)]
enum LiteralKind {
    EQ,
    LETTER,
    DIGIT,
    WHITESPACE,
    OTHER,
}

#[derive(Eq, PartialEq, Clone)]
pub struct Lexer {
    input: String,
    position: usize,
}

impl Lexer {
    pub fn next_concrete_token(&mut self) -> Token {
        let start = min(self.position, self.input.len());
        let rest = &self.input[start..];
        let ch = rest.chars().nth(0).unwrap_or('\0');

        let kind = match ch {
            '*' => TokenKind::Asterisk,
            '/' => TokenKind::Slash,
            '+' => TokenKind::Plus,
            '-' => TokenKind::Minus,

            ',' => TokenKind::Comma,
            ';' => TokenKind::Semicolon,

            '(' => TokenKind::Lparen,
            ')' => TokenKind::Rparen,

            '{' => TokenKind::Lbrace,
            '}' => TokenKind::Rbrace,
            '<' => TokenKind::Lt,
            '>' => TokenKind::Gt,

            '\0' => TokenKind::Eof,

            c => match literal_kind(c) {
                DIGIT => TokenKind::Int { value: contiguous(rest, DIGIT).parse::<i32>().unwrap() },

                WHITESPACE => TokenKind::Blank { value: contiguous(rest, WHITESPACE).into() },

                OTHER => TokenKind::Illegal { value: contiguous(rest, OTHER).into() },

                EQ => match contiguous(rest, EQ) {
                    "=" => TokenKind::Assign,
                    "!" => TokenKind::Bang,
                    "==" => TokenKind::Equals,
                    "!=" => TokenKind::Differs,
                    value => TokenKind::Illegal { value: value.into() },
                },

                LETTER => match contiguous(rest, LETTER) {
                    "let" => TokenKind::Let,
                    "fn" => TokenKind::Function,
                    "true" => TokenKind::True,
                    "false" => TokenKind::False,
                    "if" => TokenKind::If,
                    "else" => TokenKind::Else,
                    "return" => TokenKind::Return,
                    s => TokenKind::Ident { name: s.into() },
                },
            },
        };

        self.position += kind.len();

        return Token { kind, span: Span { start, end: self.position } };
    }

    pub fn next_token(&mut self) -> Token {
        loop {
            match self.next_concrete_token() {
                Token { kind: TokenKind::Blank { .. }, .. } => continue,
                token => return token,
            };
        }
    }
}

impl TokenKind {
    pub fn len(&self) -> usize {
        return match &self {
            TokenKind::Eof => 0,
            TokenKind::Equals | TokenKind::Differs | TokenKind::Function | TokenKind::If => 2,
            TokenKind::Let => 3,
            TokenKind::True | TokenKind::Else => 4,
            TokenKind::False => 5,
            TokenKind::Return => 6,

            TokenKind::Ident { name } => name.len(),
            TokenKind::Int { value } => value.to_string().len(),
            TokenKind::Illegal { value } => value.len(),
            TokenKind::Blank { value } => value.len(),
            _ => 1,
        };
    }
}

impl From<&str> for Lexer {
    fn from(value: &str) -> Self {
        return Lexer {
            input: value.into(),
            position: 0,
        };
    }
}

fn literal_kind(value: char) -> LiteralKind {
    match value {
        ' ' => WHITESPACE,
        '\t' => WHITESPACE,
        '\n' => WHITESPACE,
        '\r' => WHITESPACE,
        '_' => LETTER,
        '=' => EQ,
        '!' => EQ,
        'a'..='z' => LETTER,
        'A'..='Z' => LETTER,
        '0'..='9' => DIGIT,
        _ => OTHER,
    }
}

fn contiguous(input: &str, kind: LiteralKind) -> &str {
    let len = input
        .chars()
        .take_while(|c| literal_kind(*c) == kind)
        .count();

    return &input[..len];
}
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lexer_initialization() {
        let mut lexer = Lexer::from("a");

        assert_eq!(lexer.input, "a");
        assert_eq!(lexer.position, 0);

        assert_eq!(
            lexer.next_token(),
            Token {
                kind: TokenKind::Ident { name: "a".into() },
                span: Span { start: 0, end: 1 },
            }
        );

        assert_eq!(lexer.input, "a");
        assert_eq!(lexer.position, 1);
    }

    #[test]
    fn test_base_tokens() {
        let source = "
        =+(){},;
        ";
        let mut lexer = Lexer::from(source);

        assert_eq!(lexer.next_token().kind, TokenKind::Assign);
        assert_eq!(lexer.next_token().kind, TokenKind::Plus);
        assert_eq!(lexer.next_token().kind, TokenKind::Lparen);
        assert_eq!(lexer.next_token().kind, TokenKind::Rparen);
        assert_eq!(lexer.next_token().kind, TokenKind::Lbrace);
        assert_eq!(lexer.next_token().kind, TokenKind::Rbrace);
        assert_eq!(lexer.next_token().kind, TokenKind::Comma);
        assert_eq!(lexer.next_token().kind, TokenKind::Semicolon);
        assert_eq!(lexer.next_token().kind, TokenKind::Eof);
    }

    #[test]
    fn test_all_tokens_1() {
        let source = "
        let five = 5;
        let ten = 10;
        let add = fn(x, y) { x + y; };
        let result = add(five, ten);
        ";

        let mut lexer = Lexer::from(source);

        assert_eq!(lexer.next_token().kind, TokenKind::Let);
        assert_eq!(
            lexer.next_token().kind,
            TokenKind::Ident {
                name: "five".into()
            }
        );
        assert_eq!(lexer.next_token().kind, TokenKind::Assign);
        assert_eq!(lexer.next_token().kind, TokenKind::Int { value: 5 });
        assert_eq!(lexer.next_token().kind, TokenKind::Semicolon);

        assert_eq!(lexer.next_token().kind, TokenKind::Let);
        assert_eq!(
            lexer.next_token().kind,
            TokenKind::Ident { name: "ten".into() }
        );
        assert_eq!(lexer.next_token().kind, TokenKind::Assign);
        assert_eq!(lexer.next_token().kind, TokenKind::Int { value: 10 });
        assert_eq!(lexer.next_token().kind, TokenKind::Semicolon);

        assert_eq!(lexer.next_token().kind, TokenKind::Let);
        assert_eq!(
            lexer.next_token().kind,
            TokenKind::Ident { name: "add".into() }
        );
        assert_eq!(lexer.next_token().kind, TokenKind::Assign);
        assert_eq!(lexer.next_token().kind, TokenKind::Function);
        assert_eq!(lexer.next_token().kind, TokenKind::Lparen);
        assert_eq!(
            lexer.next_token().kind,
            TokenKind::Ident { name: "x".into() }
        );
        assert_eq!(lexer.next_token().kind, TokenKind::Comma);
        assert_eq!(
            lexer.next_token().kind,
            TokenKind::Ident { name: "y".into() }
        );
        assert_eq!(lexer.next_token().kind, TokenKind::Rparen);
        {
            assert_eq!(lexer.next_token().kind, TokenKind::Lbrace);

            assert_eq!(
                lexer.next_token().kind,
                TokenKind::Ident { name: "x".into() }
            );
            assert_eq!(lexer.next_token().kind, TokenKind::Plus);
            assert_eq!(
                lexer.next_token().kind,
                TokenKind::Ident { name: "y".into() }
            );
            assert_eq!(lexer.next_token().kind, TokenKind::Semicolon);

            assert_eq!(lexer.next_token().kind, TokenKind::Rbrace);
            assert_eq!(lexer.next_token().kind, TokenKind::Semicolon);
        }

        assert_eq!(lexer.next_token().kind, TokenKind::Let);
        assert_eq!(
            lexer.next_token().kind,
            TokenKind::Ident {
                name: "result".into()
            }
        );
        assert_eq!(lexer.next_token().kind, TokenKind::Assign);
        assert_eq!(
            lexer.next_token().kind,
            TokenKind::Ident { name: "add".into() }
        );
        assert_eq!(lexer.next_token().kind, TokenKind::Lparen);
        assert_eq!(
            lexer.next_token().kind,
            TokenKind::Ident {
                name: "five".into()
            }
        );
        assert_eq!(lexer.next_token().kind, TokenKind::Comma);
        assert_eq!(
            lexer.next_token().kind,
            TokenKind::Ident { name: "ten".into() }
        );
        assert_eq!(lexer.next_token().kind, TokenKind::Rparen);
        assert_eq!(lexer.next_token().kind, TokenKind::Semicolon);
        assert_eq!(lexer.next_token().kind, TokenKind::Eof);
    }

    #[test]
    fn test_all_tokens_2() {
        let source = "
        !-/*5;
        5 < 10 > 5;
        ";

        let mut lexer = Lexer::from(source);

        assert_eq!(lexer.next_token().kind, TokenKind::Bang);
        assert_eq!(lexer.next_token().kind, TokenKind::Minus);
        assert_eq!(lexer.next_token().kind, TokenKind::Slash);
        assert_eq!(lexer.next_token().kind, TokenKind::Asterisk);
        assert_eq!(lexer.next_token().kind, TokenKind::Int { value: 5 });
        assert_eq!(lexer.next_token().kind, TokenKind::Semicolon);

        assert_eq!(lexer.next_token().kind, TokenKind::Int { value: 5 });
        assert_eq!(lexer.next_token().kind, TokenKind::Lt);
        assert_eq!(lexer.next_token().kind, TokenKind::Int { value: 10 });
        assert_eq!(lexer.next_token().kind, TokenKind::Gt);
        assert_eq!(lexer.next_token().kind, TokenKind::Int { value: 5 });
        assert_eq!(lexer.next_token().kind, TokenKind::Semicolon);

        assert_eq!(lexer.next_token().kind, TokenKind::Eof);
    }

    #[test]
    fn test_all_tokens_3() {
        let source = "
        if (5 < 10) {
            return true;
        } else {
            return false;
        }
        ";

        let mut lexer = Lexer::from(source);

        assert_eq!(lexer.next_token().kind, TokenKind::If);
        assert_eq!(lexer.next_token().kind, TokenKind::Lparen);
        assert_eq!(lexer.next_token().kind, TokenKind::Int { value: 5 });
        assert_eq!(lexer.next_token().kind, TokenKind::Lt);
        assert_eq!(lexer.next_token().kind, TokenKind::Int { value: 10 });
        assert_eq!(lexer.next_token().kind, TokenKind::Rparen);
        assert_eq!(lexer.next_token().kind, TokenKind::Lbrace);
        {
            assert_eq!(lexer.next_token().kind, TokenKind::Return);
            assert_eq!(lexer.next_token().kind, TokenKind::True);
            assert_eq!(lexer.next_token().kind, TokenKind::Semicolon);
        }
        assert_eq!(lexer.next_token().kind, TokenKind::Rbrace);
        assert_eq!(lexer.next_token().kind, TokenKind::Else);
        assert_eq!(lexer.next_token().kind, TokenKind::Lbrace);
        {
            assert_eq!(lexer.next_token().kind, TokenKind::Return);
            assert_eq!(lexer.next_token().kind, TokenKind::False);
            assert_eq!(lexer.next_token().kind, TokenKind::Semicolon);
        }
        assert_eq!(lexer.next_token().kind, TokenKind::Rbrace);
        assert_eq!(lexer.next_token().kind, TokenKind::Eof);
    }

    #[test]
    fn test_all_tokens_4() {
        let source = "
        10 == 10;
        10 != 9;
        ";

        let mut lexer = Lexer::from(source);

        assert_eq!(lexer.next_token().kind, TokenKind::Int { value: 10 });
        assert_eq!(lexer.next_token().kind, TokenKind::Equals);
        assert_eq!(lexer.next_token().kind, TokenKind::Int { value: 10 });
        assert_eq!(lexer.next_token().kind, TokenKind::Semicolon);

        assert_eq!(lexer.next_token().kind, TokenKind::Int { value: 10 });
        assert_eq!(lexer.next_token().kind, TokenKind::Differs);
        assert_eq!(lexer.next_token().kind, TokenKind::Int { value: 9 });
        assert_eq!(lexer.next_token().kind, TokenKind::Semicolon);

        assert_eq!(lexer.next_token().kind, TokenKind::Eof);
    }
}
