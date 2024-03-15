use crate::re_lexer::TokenKind::Eof;
use std::cmp::min;
use std::ops::Index;

#[derive(Debug, Eq, PartialEq, Default, Copy, Clone)]
pub struct Range {
    pub start: usize,
    pub end: usize,
}

impl Index<Range> for str {
    type Output = str;

    fn index(&self, index: Range) -> &Self::Output {
        &self[index.start..index.end]
    }
}

impl Range {
    pub fn new(start: usize, end: usize) -> Range {
        Range { start, end }
    }
    pub fn merge(start: &Range, end: &Range) -> Range {
        Range { start: start.start, end: end.end }
    }
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum TokenKind {
    Eof,

    Bang,
    Assign,

    Comma,
    Colon,
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

    LBracket,
    RBracket,

    Equals,
    Differs,

    Function,
    Let,
    True,
    False,
    If,
    Else,
    Return,

    Str,
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
        ' ' | '\t' | '\n' | '\r' => LiteralKind::WHITESPACE,
        '_' | 'a'..='z' | 'A'..='Z' => LiteralKind::LETTER,
        '0'..='9' => LiteralKind::DIGIT,
        _ => LiteralKind::OTHER,
    }
}

pub fn raw_token_after(input: &str, range: &Range) -> RawToken {
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
        ':' => (TokenKind::Colon, 1),
        ';' => (TokenKind::Semicolon, 1),

        '(' => (TokenKind::Lparen, 1),
        ')' => (TokenKind::Rparen, 1),

        '{' => (TokenKind::Lbrace, 1),
        '}' => (TokenKind::Rbrace, 1),

        '<' => (TokenKind::Lt, 1),
        '>' => (TokenKind::Gt, 1),

        '[' => (TokenKind::LBracket, 1),
        ']' => (TokenKind::RBracket, 1),

        '!' => match rest.chars().nth(1).unwrap_or('\0') {
            '=' => (TokenKind::Differs, 2),
            _ => (TokenKind::Bang, 1),
        },

        '=' => match rest.chars().nth(1).unwrap_or('\0') {
            '=' => (TokenKind::Equals, 2),
            _ => (TokenKind::Assign, 1),
        },
        '"' => {
            let mut last = '"';
            let mut count = 1;
            for x in rest.chars().skip(1) {
                if last != '\\' {
                    if x == '"' {
                        break;
                    }
                }
                last = x;
                count += 1;
            }

            (TokenKind::Str, count + 1)
        }

        c => {
            let ckind = literal_kind(c);
            let size = rest
                .chars()
                .take_while(|c| literal_kind(*c) == ckind)
                .count();

            let tkind = match ckind {
                LiteralKind::DIGIT => TokenKind::Int,
                LiteralKind::WHITESPACE => TokenKind::Blank,
                LiteralKind::OTHER => TokenKind::Illegal,
                LiteralKind::LETTER => match &rest[..size] {
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

    RawToken {
        kind,
        range: Range { start, end: start + len },
    }
}

pub fn token_after(input: &str, range: &Range) -> RawToken {
    let mut token = raw_token_after(input, range);

    while matches!(token.kind, TokenKind::Blank) {
        token = raw_token_after(input, &token.range);
    }

    token
}

pub fn raw_tokens(input: &str) -> impl Iterator<Item = RawToken> + '_ {
    let mut current = Range::new(0, 0);
    std::iter::from_fn(move || {
        let token = raw_token_after(input, &current);
        current = token.range;
        if token.kind == Eof {
            None
        } else {
            Some(token)
        }
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Debug, Eq, PartialEq, Copy, Clone)]
    pub struct Token<'a> {
        pub kind: TokenKind,
        pub content: &'a str,
    }

    pub fn tokens(input: &str) -> impl Iterator<Item = Token<'_>> {
        raw_tokens(input)
            .filter(|t| t.kind != TokenKind::Blank)
            .map(|t| Token { kind: t.kind, content: &input[t.range] })
    }

    #[test]
    fn basic() {
        let source = "
        = + () {} ,; "
            .trim_start();

        let mut iter = tokens(source);

        assert_eq!(iter.next().unwrap().kind, TokenKind::Assign);
        assert_eq!(iter.next().unwrap().kind, TokenKind::Plus);
        assert_eq!(iter.next().unwrap().kind, TokenKind::Lparen);

        assert_eq!(iter.next().unwrap().kind, TokenKind::Rparen);
        assert_eq!(iter.next().unwrap().kind, TokenKind::Lbrace);
        assert_eq!(iter.next().unwrap().kind, TokenKind::Rbrace);
        assert_eq!(iter.next().unwrap().kind, TokenKind::Comma);
        assert_eq!(iter.next().unwrap().kind, TokenKind::Semicolon);

        assert_eq!(iter.next(), None);
    }

    #[test]
    fn operators() {
        let source = "
        ! -
        / *
        5 < 10;
        10 > 5;
        10 == 10;
        10 != 9;
        !!false
        ";

        let mut lexer = tokens(source);

        assert_eq!(lexer.next().unwrap().kind, TokenKind::Bang);
        assert_eq!(lexer.next().unwrap().kind, TokenKind::Minus);
        assert_eq!(lexer.next().unwrap().kind, TokenKind::Slash);
        assert_eq!(lexer.next().unwrap().kind, TokenKind::Asterisk);

        assert_eq!(lexer.next().unwrap().content, "5");
        assert_eq!(lexer.next().unwrap().kind, TokenKind::Lt);
        assert_eq!(lexer.next().unwrap().content, "10");
        assert_eq!(lexer.next().unwrap().kind, TokenKind::Semicolon);

        assert_eq!(lexer.next().unwrap().content, "10");
        assert_eq!(lexer.next().unwrap().kind, TokenKind::Gt);
        assert_eq!(lexer.next().unwrap().content, "5");
        assert_eq!(lexer.next().unwrap().kind, TokenKind::Semicolon);

        assert_eq!(lexer.next().unwrap().content, "10");
        assert_eq!(lexer.next().unwrap().kind, TokenKind::Equals);
        assert_eq!(lexer.next().unwrap().content, "10");
        assert_eq!(lexer.next().unwrap().kind, TokenKind::Semicolon);

        assert_eq!(lexer.next().unwrap().content, "10");
        assert_eq!(lexer.next().unwrap().kind, TokenKind::Differs);
        assert_eq!(lexer.next().unwrap().content, "9");
        assert_eq!(lexer.next().unwrap().kind, TokenKind::Semicolon);

        assert_eq!(lexer.next().unwrap().kind, TokenKind::Bang);
        assert_eq!(lexer.next().unwrap().kind, TokenKind::Bang);
        assert_eq!(lexer.next().unwrap().content, "false");

        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn let_fn() {
        let source = "
        let five = 5;
        let ten = 10;
        let add = fn(x, y) { x + y; };
        let result = add(five, ten);
        "
        .trim_start();

        let mut lexer = tokens(source);

        assert_eq!(lexer.next().unwrap().content, "let");
        assert_eq!(lexer.next().unwrap().content, "five");
        assert_eq!(lexer.next().unwrap().content, "=");
        assert_eq!(lexer.next().unwrap().content, "5");
        assert_eq!(lexer.next().unwrap().content, ";");

        assert_eq!(lexer.next().unwrap().content, "let");
        assert_eq!(lexer.next().unwrap().content, "ten");
        assert_eq!(lexer.next().unwrap().content, "=");
        assert_eq!(lexer.next().unwrap().content, "10");
        assert_eq!(lexer.next().unwrap().content, ";");

        assert_eq!(lexer.next().unwrap().content, "let");
        assert_eq!(lexer.next().unwrap().content, "add");
        assert_eq!(lexer.next().unwrap().content, "=");
        assert_eq!(lexer.next().unwrap().content, "fn");
        assert_eq!(lexer.next().unwrap().content, "(");
        assert_eq!(lexer.next().unwrap().content, "x");
        assert_eq!(lexer.next().unwrap().content, ",");
        assert_eq!(lexer.next().unwrap().content, "y");
        assert_eq!(lexer.next().unwrap().content, ")");
        assert_eq!(lexer.next().unwrap().content, "{");
        assert_eq!(lexer.next().unwrap().content, "x");
        assert_eq!(lexer.next().unwrap().content, "+");
        assert_eq!(lexer.next().unwrap().content, "y");
        assert_eq!(lexer.next().unwrap().content, ";");
        assert_eq!(lexer.next().unwrap().content, "}");
        assert_eq!(lexer.next().unwrap().content, ";");

        assert_eq!(lexer.next().unwrap().content, "let");
        assert_eq!(lexer.next().unwrap().content, "result");
        assert_eq!(lexer.next().unwrap().content, "=");
        assert_eq!(lexer.next().unwrap().content, "add");
        assert_eq!(lexer.next().unwrap().content, "(");
        assert_eq!(lexer.next().unwrap().content, "five");
        assert_eq!(lexer.next().unwrap().content, ",");
        assert_eq!(lexer.next().unwrap().content, "ten");
        assert_eq!(lexer.next().unwrap().content, ")");
        assert_eq!(lexer.next().unwrap().content, ";");
    }

    #[test]
    fn if_return() {
        let source = "
        if (5 < 10) {
            return true;
        } else {
            return false;
        }
        ";

        let mut lexer = tokens(source);

        assert_eq!(lexer.next().unwrap().kind, TokenKind::If);
        assert_eq!(lexer.next().unwrap().kind, TokenKind::Lparen);
        assert_eq!(lexer.next().unwrap().content, "5");
        assert_eq!(lexer.next().unwrap().kind, TokenKind::Lt);
        assert_eq!(lexer.next().unwrap().content, "10");
        assert_eq!(lexer.next().unwrap().kind, TokenKind::Rparen);
        assert_eq!(lexer.next().unwrap().kind, TokenKind::Lbrace);
        {
            assert_eq!(lexer.next().unwrap().kind, TokenKind::Return);
            assert_eq!(lexer.next().unwrap().kind, TokenKind::True);
            assert_eq!(lexer.next().unwrap().kind, TokenKind::Semicolon);
        }
        assert_eq!(lexer.next().unwrap().kind, TokenKind::Rbrace);
        assert_eq!(lexer.next().unwrap().kind, TokenKind::Else);
        assert_eq!(lexer.next().unwrap().kind, TokenKind::Lbrace);
        {
            assert_eq!(lexer.next().unwrap().kind, TokenKind::Return);
            assert_eq!(lexer.next().unwrap().kind, TokenKind::False);
            assert_eq!(lexer.next().unwrap().kind, TokenKind::Semicolon);
        }
        assert_eq!(lexer.next().unwrap().kind, TokenKind::Rbrace);
        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn strings() {
        let source = r#"
        let a = "hey there"
        let b = "hey \"you\" there"
        "#;

        let mut lexer = tokens(source);

        assert_eq!(lexer.next().unwrap().kind, TokenKind::Let);
        assert_eq!(lexer.next().unwrap().content, "a");
        assert_eq!(lexer.next().unwrap().kind, TokenKind::Assign);
        {
            let token = lexer.next().unwrap();
            assert_eq!(token.kind, TokenKind::Str);
            assert_eq!(token.content, "\"hey there\"");
        }

        assert_eq!(lexer.next().unwrap().kind, TokenKind::Let);
        assert_eq!(lexer.next().unwrap().content, "b");
        assert_eq!(lexer.next().unwrap().kind, TokenKind::Assign);
        {
            let token = lexer.next().unwrap();
            assert_eq!(token.kind, TokenKind::Str);
            assert_eq!(token.content, "\"hey \\\"you\\\" there\"");
        }
    }

    #[test]
    fn array() {
        let source = r#"
        let a = [1 , 2 , "b"]
        "#;

        let mut lexer = tokens(source);

        assert_eq!(lexer.next().unwrap().kind, TokenKind::Let);
        assert_eq!(lexer.next().unwrap().content, "a");
        assert_eq!(lexer.next().unwrap().kind, TokenKind::Assign);
        assert_eq!(lexer.next().unwrap().kind, TokenKind::LBracket);
        assert_eq!(lexer.next().unwrap().content, "1");
        assert_eq!(lexer.next().unwrap().kind, TokenKind::Comma);
        assert_eq!(lexer.next().unwrap().content, "2");
        assert_eq!(lexer.next().unwrap().kind, TokenKind::Comma);
        assert_eq!(lexer.next().unwrap().content, "\"b\"");
        assert_eq!(lexer.next().unwrap().kind, TokenKind::RBracket);
    }

    #[test]
    fn hash() {
        let source = r#"
        let a = {"a" : 1 , "b": 2 }
        "#;

        let mut lexer = tokens(source);

        assert_eq!(lexer.next().unwrap().kind, TokenKind::Let);
        assert_eq!(lexer.next().unwrap().content, "a");
        assert_eq!(lexer.next().unwrap().kind, TokenKind::Assign);
        assert_eq!(lexer.next().unwrap().kind, TokenKind::Lbrace);
        assert_eq!(lexer.next().unwrap().content, "\"a\"");
        assert_eq!(lexer.next().unwrap().kind, TokenKind::Colon);
        assert_eq!(lexer.next().unwrap().content, "1");
        assert_eq!(lexer.next().unwrap().kind, TokenKind::Comma);
        assert_eq!(lexer.next().unwrap().content, "\"b\"");
        assert_eq!(lexer.next().unwrap().kind, TokenKind::Colon);
        assert_eq!(lexer.next().unwrap().content, "2");
        assert_eq!(lexer.next().unwrap().kind, TokenKind::Rbrace);
    }
}
