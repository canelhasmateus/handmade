use std::cell::RefCell;
use std::cmp::min;
use std::ops::Index;
use std::slice::SliceIndex;

use crate::lexer2::LiteralKind::{DIGIT, LETTER, OTHER, WHITESPACE};
use crate::lexer2::TokenKind::{Assign, Bang, Differs, Equals};

#[derive(Debug, Eq, PartialEq, Copy, Clone, Default)]
pub struct Range {
    pub start: usize,
    pub end: usize,
}

impl Index<Range> for str {
    type Output = str;

    fn index(&self, index: Range) -> &Self::Output {
        return &self[index.start..index.end];
    }
}

impl Range {
    fn new(start: usize, end: usize) -> Range {
        return Range { start, end };
    }
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
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

    Ident,
    Int,

    Illegal,
    Blank,
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub struct RawToken {
    pub kind: TokenKind,
    pub range: Range,
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

pub fn concrete_token_after(input: &str, range: &Range) -> RawToken {
    let start = min(range.end, input.len());
    let rest = &input[start..];
    let ch = rest.chars().nth(0).unwrap_or('\0');

    let (kind, len) = match ch {
        '\0' => (TokenKind::Eof, 0),

        '*' => (TokenKind::Asterisk, 1),
        '/' => (TokenKind::Slash, 1),

        '+' => (TokenKind::Plus, 1),
        '-' => (TokenKind::Minus, 1),

        ',' => (TokenKind::Comma, 1),
        ';' => (TokenKind::Semicolon, 1),

        '(' => (TokenKind::Lparen, 1),
        ')' => (TokenKind::Rparen, 1),

        '{' => (TokenKind::Lbrace, 1),
        '}' => (TokenKind::Rbrace, 1),

        '<' => (TokenKind::Lt, 1),
        '>' => (TokenKind::Gt, 1),

        '!' => match rest.chars().nth(1).unwrap_or('\0') {
            '=' => (Differs, 2),
            _ => (Bang, 1),
        },

        '=' => match rest.chars().nth(1).unwrap_or('\0') {
            '=' => (Equals, 2),
            _ => (Assign, 1),
        },

        c => {
            let ckind = literal_kind(c);
            let size = rest
                .chars()
                .take_while(|c| literal_kind(*c) == ckind)
                .count();

            let tkind = match ckind {
                DIGIT => TokenKind::Int,
                WHITESPACE => TokenKind::Blank,
                OTHER => TokenKind::Illegal,
                LETTER => match &rest[..size] {
                    "let" => TokenKind::Let,
                    "fn" => TokenKind::Function,
                    "true" => TokenKind::True,
                    "false" => TokenKind::False,
                    "if" => TokenKind::If,
                    "else" => TokenKind::Else,
                    "return" => TokenKind::Return,
                    _ => TokenKind::Ident,
                },
            };
            (tkind, size)
        }
    };

    return RawToken { kind, range: Range { start: start, end: start + len } };
}

pub fn semantic_token_after(input: &str, range: &Range) -> RawToken {
    let mut current = range;
    let mut token = concrete_token_after(input, current);
    while matches!(token.kind, TokenKind::Blank) {
        current = &token.range;
        token = concrete_token_after(input, current);
    }
    return token;
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub struct Token<'a> {
    pub kind: TokenKind,
    pub range: Range,
    pub content: &'a str,
}

#[derive(Eq, PartialEq)]
pub struct Lexer<'a> {
    input: &'a str,
    position: RefCell<Range>,
}

impl Lexer<'_> {
    pub fn lookup(&self, token: &RawToken) -> Token<'_> {
        return Token {
            kind: token.kind,
            range: token.range,
            content: &self.input[token.range],
        };
    }

    pub fn next_raw(&self) -> RawToken {
        let range = self.position.take();
        let token = semantic_token_after(self.input, &range);
        self.position.replace(token.range);
        return token;
    }

    pub fn next_token(&self) -> Token {
        let token = self.next_raw();
        return self.lookup(&token);
    }
}

impl<'a> From<&'a str> for Lexer<'a> {
    fn from(value: &'a str) -> Self {
        return Lexer { input: value, position: Range::default().into() };
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lexer_initialization() {
        let lexer = Lexer::from("a");

        assert_eq!(lexer.input, "a");
        assert_eq!(lexer.position, RefCell::from(Range::new(0, 0)));

        assert_eq!(
            lexer.next_token(),
            Token {
                kind: TokenKind::Ident,
                range: Range::new(0, 1),
                content: "a",
            }
        );

        assert_eq!(lexer.input, "a");
        assert_eq!(lexer.position, RefCell::from(Range::new(0, 1)));
    }

    #[test]
    fn test_base_tokens() {
        let source = "
        =+(){},;
        "
        .trim_start();

        let lexer = Lexer::from(source);

        assert_eq!(
            lexer.next_token(),
            Token {
                kind: TokenKind::Assign,
                range: Range::new(0, 1),
                content: "=",
            }
        );

        assert_eq!(
            lexer.next_token(),
            Token { kind: TokenKind::Plus, range: Range::new(1, 2), content: "+" }
        );

        assert_eq!(
            lexer.next_token(),
            Token {
                kind: TokenKind::Lparen,
                range: Range::new(2, 3),
                content: "(",
            }
        );

        assert_eq!(
            lexer.next_token(),
            Token {
                kind: TokenKind::Rparen,
                range: Range::new(3, 4),
                content: ")",
            }
        );

        assert_eq!(
            lexer.next_token(),
            Token {
                kind: TokenKind::Lbrace,
                range: Range::new(4, 5),
                content: "{",
            }
        );

        assert_eq!(
            lexer.next_token(),
            Token {
                kind: TokenKind::Rbrace,
                range: Range::new(5, 6),
                content: "}",
            }
        );

        assert_eq!(
            lexer.next_token(),
            Token {
                kind: TokenKind::Comma,
                range: Range::new(6, 7),
                content: ",",
            }
        );

        assert_eq!(
            lexer.next_token(),
            Token {
                kind: TokenKind::Semicolon,
                range: Range::new(7, 8),
                content: ";",
            }
        );

        assert_eq!(
            lexer.next_token(),
            Token { kind: TokenKind::Eof, range: Range::new(17, 17), content: "" }
        );
    }

    #[test]
    fn operators() {
        let source = "
        !-/*5;
        5 < 10 > 5;
        ";

        let lexer = Lexer::from(source);

        assert_eq!(lexer.next_token().kind, TokenKind::Bang);
        assert_eq!(lexer.next_token().kind, TokenKind::Minus);
        assert_eq!(lexer.next_token().kind, TokenKind::Slash);
        assert_eq!(lexer.next_token().kind, TokenKind::Asterisk);
        assert_eq!(lexer.next_token().content, "5");
        assert_eq!(lexer.next_token().kind, TokenKind::Semicolon);

        assert_eq!(lexer.next_token().content, "5");
        assert_eq!(lexer.next_token().kind, TokenKind::Lt);
        assert_eq!(lexer.next_token().content, "10");
        assert_eq!(lexer.next_token().kind, TokenKind::Gt);
        assert_eq!(lexer.next_token().content, "5");
        assert_eq!(lexer.next_token().kind, TokenKind::Semicolon);

        assert_eq!(lexer.next_token().kind, TokenKind::Eof);
    }

    #[test]
    fn test_with_ranges_1() {
        let source = "
        let five = 5;
        let ten = 10;
        let add = fn(x, y) { x + y; };
        let result = add(five, ten);
        "
        .trim_start();

        let lexer = Lexer::from(source);

        assert_eq!(
            lexer.next_token(),
            Token {
                kind: TokenKind::Let,
                range: Range::new(0, 3),
                content: "let",
            }
        );

        assert_eq!(
            lexer.next_token(),
            Token {
                kind: TokenKind::Ident,
                range: Range::new(4, 8),
                content: "five",
            }
        );

        assert_eq!(
            lexer.next_token(),
            Token {
                kind: TokenKind::Assign,
                range: Range::new(9, 10),
                content: "=",
            }
        );

        assert_eq!(
            lexer.next_token(),
            Token {
                kind: TokenKind::Int,
                range: Range::new(11, 12),
                content: "5",
            }
        );

        assert_eq!(
            lexer.next_token(),
            Token {
                kind: TokenKind::Semicolon,
                range: Range::new(12, 13),
                content: ";",
            }
        );

        assert_eq!(
            lexer.next_token(),
            Token {
                kind: TokenKind::Let,
                range: Range::new(22, 25),
                content: "let",
            }
        );

        assert_eq!(
            lexer.next_token(),
            Token {
                kind: TokenKind::Ident,
                range: Range::new(26, 29),
                content: "ten",
            }
        );

        assert_eq!(
            lexer.next_token(),
            Token {
                kind: TokenKind::Assign,
                range: Range::new(30, 31),
                content: "=",
            }
        );

        assert_eq!(
            lexer.next_token(),
            Token {
                kind: TokenKind::Int,
                range: Range::new(32, 34),
                content: "10",
            }
        );

        assert_eq!(
            lexer.next_token(),
            Token {
                kind: TokenKind::Semicolon,
                range: Range::new(34, 35),
                content: ";",
            }
        );

        assert_eq!(
            lexer.next_token(),
            Token {
                kind: TokenKind::Let,
                range: Range::new(44, 47),
                content: "let",
            }
        );

        assert_eq!(
            lexer.next_token(),
            Token {
                kind: TokenKind::Ident,
                range: Range::new(48, 51),
                content: "add",
            }
        );

        assert_eq!(
            lexer.next_token(),
            Token {
                kind: TokenKind::Assign,
                range: Range::new(52, 53),
                content: "=",
            }
        );

        assert_eq!(
            lexer.next_token(),
            Token {
                kind: TokenKind::Function,
                range: Range::new(54, 56),
                content: "fn",
            }
        );

        assert_eq!(
            lexer.next_token(),
            Token {
                kind: TokenKind::Lparen,
                range: Range::new(56, 57),
                content: "(",
            }
        );

        assert_eq!(
            lexer.next_token(),
            Token {
                kind: TokenKind::Ident,
                range: Range::new(57, 58),
                content: "x",
            }
        );

        assert_eq!(
            lexer.next_token(),
            Token {
                kind: TokenKind::Comma,
                range: Range::new(58, 59),
                content: ",",
            }
        );

        assert_eq!(
            lexer.next_token(),
            Token {
                kind: TokenKind::Ident,
                range: Range::new(60, 61),
                content: "y",
            }
        );

        assert_eq!(
            lexer.next_token(),
            Token {
                kind: TokenKind::Rparen,
                range: Range::new(61, 62),
                content: ")",
            }
        );

        assert_eq!(
            lexer.next_token(),
            Token {
                kind: TokenKind::Lbrace,
                range: Range::new(63, 64),
                content: "{",
            }
        );
        assert_eq!(
            lexer.next_token(),
            Token {
                kind: TokenKind::Ident,
                range: Range::new(65, 66),
                content: "x",
            }
        );
        assert_eq!(
            lexer.next_token(),
            Token {
                kind: TokenKind::Plus,
                range: Range::new(67, 68),
                content: "+",
            }
        );
        assert_eq!(
            lexer.next_token(),
            Token {
                kind: TokenKind::Ident,
                range: Range::new(69, 70),
                content: "y",
            }
        );
        assert_eq!(
            lexer.next_token(),
            Token {
                kind: TokenKind::Semicolon,
                range: Range::new(70, 71),
                content: ";",
            }
        );
        assert_eq!(
            lexer.next_token(),
            Token {
                kind: TokenKind::Rbrace,
                range: Range::new(72, 73),
                content: "}",
            }
        );
        assert_eq!(
            lexer.next_token(),
            Token {
                kind: TokenKind::Semicolon,
                range: Range::new(73, 74),
                content: ";",
            }
        );

        assert_eq!(
            lexer.next_token(),
            Token {
                kind: TokenKind::Let,
                range: Range::new(83, 86),
                content: "let",
            }
        );

        assert_eq!(
            lexer.next_token(),
            Token {
                kind: TokenKind::Ident,
                range: Range::new(87, 93),
                content: "result",
            }
        );

        assert_eq!(
            lexer.next_token(),
            Token {
                kind: TokenKind::Assign,
                range: Range::new(94, 95),
                content: "=",
            }
        );
        assert_eq!(
            lexer.next_token(),
            Token {
                kind: TokenKind::Ident,
                range: Range::new(96, 99),
                content: "add",
            }
        );

        assert_eq!(
            lexer.next_token(),
            Token {
                kind: TokenKind::Lparen,
                range: Range::new(99, 100),
                content: "(",
            }
        );

        assert_eq!(
            lexer.next_token(),
            Token {
                kind: TokenKind::Ident,
                range: Range::new(100, 104),
                content: "five",
            }
        );

        assert_eq!(
            lexer.next_token(),
            Token {
                kind: TokenKind::Comma,
                range: Range::new(104, 105),
                content: ",",
            }
        );
        assert_eq!(
            lexer.next_token(),
            Token {
                kind: TokenKind::Ident,
                range: Range::new(106, 109),
                content: "ten",
            }
        );
        assert_eq!(
            lexer.next_token(),
            Token {
                kind: TokenKind::Rparen,
                range: Range::new(109, 110),
                content: ")",
            }
        );
        assert_eq!(
            lexer.next_token(),
            Token {
                kind: TokenKind::Semicolon,
                range: Range::new(110, 111),
                content: ";",
            }
        );
        assert_eq!(
            lexer.next_token(),
            Token {
                kind: TokenKind::Eof,
                range: Range::new(120, 120),
                content: "",
            }
        );
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

        assert_eq!(lexer.next_token().kind, TokenKind::If);
        assert_eq!(lexer.next_token().kind, TokenKind::Lparen);
        assert_eq!(lexer.next_token().content, "5");
        assert_eq!(lexer.next_token().kind, TokenKind::Lt);
        assert_eq!(lexer.next_token().content, "10");
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

        let lexer = Lexer::from(source);

        assert_eq!(lexer.next_token().content, "10");
        assert_eq!(lexer.next_token().kind, TokenKind::Equals);
        assert_eq!(lexer.next_token().content, "10");
        assert_eq!(lexer.next_token().kind, TokenKind::Semicolon);

        assert_eq!(lexer.next_token().content, "10");
        assert_eq!(lexer.next_token().kind, TokenKind::Differs);
        assert_eq!(lexer.next_token().content, "9");
        assert_eq!(lexer.next_token().kind, TokenKind::Semicolon);

        assert_eq!(lexer.next_token().kind, TokenKind::Eof);
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

        assert_eq!(lexer.next_token().kind, TokenKind::Assign);
        assert_eq!(lexer.next_token().kind, TokenKind::Bang);

        assert_eq!(lexer.next_token().kind, TokenKind::Equals);
        assert_eq!(lexer.next_token().kind, TokenKind::Differs);

        assert_eq!(lexer.next_token().kind, TokenKind::Bang);
        assert_eq!(lexer.next_token().kind, TokenKind::Bang);
        assert_eq!(lexer.next_token().kind, TokenKind::False);

        assert_eq!(lexer.next_token().kind, TokenKind::Eof);
    }
}
