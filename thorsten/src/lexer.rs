use std::cell::RefCell;
use std::cmp::min;

use crate::lexer::LiteralKind::{DIGIT, LETTER, OTHER, WHITESPACE};
use crate::lexer::TokenKind::{Assign, Bang, Differs, Equals};

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
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
pub struct Lexer {
    input: String,
    pub position: RefCell<usize>,
}

impl Lexer {
    pub fn slice(&self, span: &Span) -> &str {
        return &self.input[span.start..span.end];
    }
    pub fn concrete_token_after(&self, span: &Span) -> Token {
        let start = min(span.end, self.input.len());
        let rest = &self.input[start..];
        let ch = rest.chars().nth(0).unwrap_or('\0');

        let kind = match ch {
            '\0' => TokenKind::Eof,

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
            '!' => match rest.chars().nth(1).unwrap_or('\0') {
                '=' => Differs,
                _ => Bang,
            },
            '=' => match rest.chars().nth(1).unwrap_or('\0') {
                '=' => Equals,
                _ => Assign,
            },

            _ => match contiguous(rest) {
                (DIGIT, content) => TokenKind::Int { value: content.parse::<i32>().unwrap() },
                (WHITESPACE, content) => TokenKind::Blank { value: content.into() },
                (OTHER, content) => TokenKind::Illegal { value: content.into() },
                (LETTER, content) => match content {
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

        let end = start + kind.len();
        return Token { kind, span: Span { start, end } };
    }
    pub fn semantic_token_after(&self, span: &Span) -> Token {
        let mut current = self.concrete_token_after(span);
        while matches!(current.kind, TokenKind::Blank { .. }) {
            current = self.concrete_token_after(&current.span);
        }
        return current;
    }

    pub fn move_to(&self, span: &Span) {
        self.position.replace(span.end);
    }

    pub fn next_concrete(&self) -> Token {
        let token = self.concrete_token_after(&Span { start: 0, end: self.position.take() });
        self.move_to(&token.span);
        return token;
    }

    pub fn next_semantic(&self) -> Token {
        loop {
            match self.next_concrete() {
                Token { kind: TokenKind::Blank { .. }, .. } => continue,
                token => return token,
            };
        }
    }
}

impl Token {
    pub fn len(&self) -> usize {
        return self.kind.len();
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

impl<'a> From<&'a str> for Lexer {
    fn from(value: &'a str) -> Self {
        return Lexer { input: value.into(), position: 0.into() };
    }
}

#[derive(Eq, PartialEq)]
enum LiteralKind {
    LETTER,
    DIGIT,
    WHITESPACE,
    OTHER,
}

fn literal_kind(value: char) -> LiteralKind {
    match value {
        ' ' | '\t' | '\n' | '\r' => WHITESPACE,
        '_' | 'a'..='z' | 'A'..='Z' => LETTER,
        '0'..='9' => DIGIT,
        _ => OTHER,
    }
}

fn contiguous(input: &str) -> (LiteralKind, &str) {
    let first = input.chars().nth(0).unwrap_or('\0');
    let kind = literal_kind(first);
    let len = input
        .chars()
        .take_while(|c| literal_kind(*c) == kind)
        .count();

    return (kind, &input[..len]);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lexer_initialization() {
        let lexer = Lexer::from("a");

        assert_eq!(lexer.input, "a");
        assert_eq!(lexer.position.take(), 0);

        assert_eq!(
            lexer.next_semantic(),
            Token {
                kind: TokenKind::Ident { name: "a".into() },
                span: Span { start: 0, end: 1 },
            }
        );

        assert_eq!(lexer.input, "a");
        assert_eq!(lexer.position.take(), 1);
    }

    #[test]
    fn test_base_tokens() {
        let source = "
        =+(){},;
        ";
        let lexer = Lexer::from(source);

        assert_eq!(lexer.next_semantic().kind, TokenKind::Assign);
        assert_eq!(lexer.next_semantic().kind, TokenKind::Plus);
        assert_eq!(lexer.next_semantic().kind, TokenKind::Lparen);
        assert_eq!(lexer.next_semantic().kind, TokenKind::Rparen);
        assert_eq!(lexer.next_semantic().kind, TokenKind::Lbrace);
        assert_eq!(lexer.next_semantic().kind, TokenKind::Rbrace);
        assert_eq!(lexer.next_semantic().kind, TokenKind::Comma);
        assert_eq!(lexer.next_semantic().kind, TokenKind::Semicolon);
        assert_eq!(lexer.next_semantic().kind, TokenKind::Eof);
    }

    #[test]
    fn test_all_tokens_1() {
        let source = "
        let five = 5;
        let ten = 10;
        let add = fn(x, y) { x + y; };
        let result = add(five, ten);
        ";

        let lexer = Lexer::from(source);

        assert_eq!(lexer.next_semantic().kind, TokenKind::Let);
        assert_eq!(
            lexer.next_semantic().kind,
            TokenKind::Ident { name: "five".into() }
        );
        assert_eq!(lexer.next_semantic().kind, TokenKind::Assign);
        assert_eq!(lexer.next_semantic().kind, TokenKind::Int { value: 5 });
        assert_eq!(lexer.next_semantic().kind, TokenKind::Semicolon);

        assert_eq!(lexer.next_semantic().kind, TokenKind::Let);
        assert_eq!(
            lexer.next_semantic().kind,
            TokenKind::Ident { name: "ten".into() }
        );
        assert_eq!(lexer.next_semantic().kind, TokenKind::Assign);
        assert_eq!(lexer.next_semantic().kind, TokenKind::Int { value: 10 });
        assert_eq!(lexer.next_semantic().kind, TokenKind::Semicolon);

        assert_eq!(lexer.next_semantic().kind, TokenKind::Let);
        assert_eq!(
            lexer.next_semantic().kind,
            TokenKind::Ident { name: "add".into() }
        );
        assert_eq!(lexer.next_semantic().kind, TokenKind::Assign);
        assert_eq!(lexer.next_semantic().kind, TokenKind::Function);
        assert_eq!(lexer.next_semantic().kind, TokenKind::Lparen);
        assert_eq!(
            lexer.next_semantic().kind,
            TokenKind::Ident { name: "x".into() }
        );
        assert_eq!(lexer.next_semantic().kind, TokenKind::Comma);
        assert_eq!(
            lexer.next_semantic().kind,
            TokenKind::Ident { name: "y".into() }
        );
        assert_eq!(lexer.next_semantic().kind, TokenKind::Rparen);
        {
            assert_eq!(lexer.next_semantic().kind, TokenKind::Lbrace);

            assert_eq!(
                lexer.next_semantic().kind,
                TokenKind::Ident { name: "x".into() }
            );
            assert_eq!(lexer.next_semantic().kind, TokenKind::Plus);
            assert_eq!(
                lexer.next_semantic().kind,
                TokenKind::Ident { name: "y".into() }
            );
            assert_eq!(lexer.next_semantic().kind, TokenKind::Semicolon);

            assert_eq!(lexer.next_semantic().kind, TokenKind::Rbrace);
            assert_eq!(lexer.next_semantic().kind, TokenKind::Semicolon);
        }

        assert_eq!(lexer.next_semantic().kind, TokenKind::Let);
        assert_eq!(
            lexer.next_semantic().kind,
            TokenKind::Ident { name: "result".into() }
        );
        assert_eq!(lexer.next_semantic().kind, TokenKind::Assign);
        assert_eq!(
            lexer.next_semantic().kind,
            TokenKind::Ident { name: "add".into() }
        );
        assert_eq!(lexer.next_semantic().kind, TokenKind::Lparen);
        assert_eq!(
            lexer.next_semantic().kind,
            TokenKind::Ident { name: "five".into() }
        );
        assert_eq!(lexer.next_semantic().kind, TokenKind::Comma);
        assert_eq!(
            lexer.next_semantic().kind,
            TokenKind::Ident { name: "ten".into() }
        );
        assert_eq!(lexer.next_semantic().kind, TokenKind::Rparen);
        assert_eq!(lexer.next_semantic().kind, TokenKind::Semicolon);
        assert_eq!(lexer.next_semantic().kind, TokenKind::Eof);
    }

    #[test]
    fn test_all_tokens_2() {
        let source = "
        !-/*5;
        5 < 10 > 5;
        ";

        let lexer = Lexer::from(source);

        assert_eq!(lexer.next_semantic().kind, TokenKind::Bang);
        assert_eq!(lexer.next_semantic().kind, TokenKind::Minus);
        assert_eq!(lexer.next_semantic().kind, TokenKind::Slash);
        assert_eq!(lexer.next_semantic().kind, TokenKind::Asterisk);
        assert_eq!(lexer.next_semantic().kind, TokenKind::Int { value: 5 });
        assert_eq!(lexer.next_semantic().kind, TokenKind::Semicolon);

        assert_eq!(lexer.next_semantic().kind, TokenKind::Int { value: 5 });
        assert_eq!(lexer.next_semantic().kind, TokenKind::Lt);
        assert_eq!(lexer.next_semantic().kind, TokenKind::Int { value: 10 });
        assert_eq!(lexer.next_semantic().kind, TokenKind::Gt);
        assert_eq!(lexer.next_semantic().kind, TokenKind::Int { value: 5 });
        assert_eq!(lexer.next_semantic().kind, TokenKind::Semicolon);

        assert_eq!(lexer.next_semantic().kind, TokenKind::Eof);
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

        let lexer = Lexer::from(source);

        assert_eq!(lexer.next_semantic().kind, TokenKind::If);
        assert_eq!(lexer.next_semantic().kind, TokenKind::Lparen);
        assert_eq!(lexer.next_semantic().kind, TokenKind::Int { value: 5 });
        assert_eq!(lexer.next_semantic().kind, TokenKind::Lt);
        assert_eq!(lexer.next_semantic().kind, TokenKind::Int { value: 10 });
        assert_eq!(lexer.next_semantic().kind, TokenKind::Rparen);
        assert_eq!(lexer.next_semantic().kind, TokenKind::Lbrace);
        {
            assert_eq!(lexer.next_semantic().kind, TokenKind::Return);
            assert_eq!(lexer.next_semantic().kind, TokenKind::True);
            assert_eq!(lexer.next_semantic().kind, TokenKind::Semicolon);
        }
        assert_eq!(lexer.next_semantic().kind, TokenKind::Rbrace);
        assert_eq!(lexer.next_semantic().kind, TokenKind::Else);
        assert_eq!(lexer.next_semantic().kind, TokenKind::Lbrace);
        {
            assert_eq!(lexer.next_semantic().kind, TokenKind::Return);
            assert_eq!(lexer.next_semantic().kind, TokenKind::False);
            assert_eq!(lexer.next_semantic().kind, TokenKind::Semicolon);
        }
        assert_eq!(lexer.next_semantic().kind, TokenKind::Rbrace);
        assert_eq!(lexer.next_semantic().kind, TokenKind::Eof);
    }

    #[test]
    fn test_all_tokens_4() {
        let source = "
        10 == 10;
        10 != 9;
        ";

        let lexer = Lexer::from(source);

        assert_eq!(lexer.next_semantic().kind, TokenKind::Int { value: 10 });
        assert_eq!(lexer.next_semantic().kind, TokenKind::Equals);
        assert_eq!(lexer.next_semantic().kind, TokenKind::Int { value: 10 });
        assert_eq!(lexer.next_semantic().kind, TokenKind::Semicolon);

        assert_eq!(lexer.next_semantic().kind, TokenKind::Int { value: 10 });
        assert_eq!(lexer.next_semantic().kind, TokenKind::Differs);
        assert_eq!(lexer.next_semantic().kind, TokenKind::Int { value: 9 });
        assert_eq!(lexer.next_semantic().kind, TokenKind::Semicolon);

        assert_eq!(lexer.next_semantic().kind, TokenKind::Eof);
    }

    #[test]
    fn regression_doubles() {
        let source = "
        =
        !

        ==
        !=

        !!false
        ";

        let lexer = Lexer::from(source);

        assert_eq!(lexer.next_semantic().kind, TokenKind::Assign);
        assert_eq!(lexer.next_semantic().kind, TokenKind::Bang);

        assert_eq!(lexer.next_semantic().kind, TokenKind::Equals);
        assert_eq!(lexer.next_semantic().kind, TokenKind::Differs);

        assert_eq!(lexer.next_semantic().kind, TokenKind::Bang);
        assert_eq!(lexer.next_semantic().kind, TokenKind::Bang);
        assert_eq!(lexer.next_semantic().kind, TokenKind::False);

        assert_eq!(lexer.next_semantic().kind, TokenKind::Eof);
    }
}
