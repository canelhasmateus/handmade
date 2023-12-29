use std::ops::Index;

use crate::lexer::LiteralKind::{DIGIT, LETTER, OTHER, WHITESPACE};

fn from_ascii(name: &[u8]) -> String {
    return std::str::from_utf8(name).unwrap().to_string()
}

fn from_ascii_vec(name: Vec<u8>) -> String {
    return String::from_utf8(name).unwrap(); // todo
}

#[derive(Debug, Eq, PartialEq)]
enum Token {
    EOF,
    ASSIGN,
    PLUS,
    COMMA,
    SEMICOLON,
    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,
    FUNCTION,
    LET,
    ILLEGAL { value: String },
    IDENT { value: String },
    INT { value: i32 },
    WHITESPACE { value: String }
}

#[derive(Eq, PartialEq)]
enum LiteralKind {
    LETTER,
    DIGIT,
    WHITESPACE,
    OTHER,
}

impl From<char> for LiteralKind {
    fn from(value: char) -> Self {
        match value {
            ' ' => WHITESPACE,
            '\t' => WHITESPACE,
            '\n' => WHITESPACE,
            '\r' => WHITESPACE,
            '_' => LETTER,
            'a'..='z' => LETTER,
            'A'..='Z' => LETTER,
            '0'..='9' => DIGIT,
            _ => OTHER
        }
    }
}
pub struct Lexer {
    input: Vec<u8>,
    ch: char,
    position: usize,
    read_position: usize,
}

impl From<&str> for Lexer {
    fn from(value: &str) -> Self {
        return Lexer {
            input: value.as_bytes().to_owned(), // todo
            ch: '\0',
            position: 0,
            read_position: 0,
        };
    }
}

impl Lexer {
    fn read_char(&mut self) {
        self.ch = if self.read_position >= self.input.len() {
            '\0'
        } else {
            self.input[self.read_position] as char
        };

        self.position = self.read_position;
        self.read_position += 1;
    }

    fn next_token(&mut self) -> Token {
        loop {
            let token = self.next_real_token();
            if let Token::WHITESPACE { value: _ } = token {
                continue
            } else {
                return token
            }
        }
    }

    fn next_real_token(&mut self) -> Token {
        self.read_char();
        return match self.ch {
            '=' => Token::ASSIGN,
            '+' => Token::PLUS,

            ',' => Token::COMMA,
            ';' => Token::SEMICOLON,

            '(' => Token::LPAREN,
            ')' => Token::RPAREN,

            '{' => Token::LBRACE,
            '}' => Token::RBRACE,

            '\0' => Token::EOF,
            c => {
                match LiteralKind::from(c) {
                    LETTER => {
                        let name = self.agglomerate(&LETTER);
                        match name[..] {
                            [b'l', b'e', b't'] => Token::LET,
                            [b'f', b'n'] => Token::FUNCTION,
                            _ => Token::IDENT { value: from_ascii_vec(name) }
                        }
                    }

                    DIGIT => {
                        Token::INT {
                            value: self.agglomerate(&DIGIT)
                                .iter()
                                .map(|d| *d as char)
                                .collect::<String>()
                                .parse::<i32>()
                                .unwrap()
                        }
                    }

                    WHITESPACE => {
                        Token::WHITESPACE {
                            value: from_ascii_vec(self.agglomerate(&WHITESPACE))
                        }
                    }

                    OTHER => {
                        Token::ILLEGAL { value: from_ascii_vec(self.agglomerate(&OTHER)) }
                    }
                }
            }
        };
    }

    fn agglomerate(&mut self, kind: &LiteralKind) -> Vec<u8> {
        let initial_pos = self.position;
        while let Some(_) = self.peek(kind) {
            self.read_char();
        }

        return self.input[initial_pos..=self.position].to_vec();
    }

    fn peek(&mut self, kind: &LiteralKind) -> Option<u8> {
        let idx = self.position + 1;
        if idx >= self.input.len() {
            return None;
        }
        let ch = self.input[idx];
        let expected = LiteralKind::from(ch as char);
        return match expected {
            c if &c == kind => Some(ch),
            _ => None
        };
    }
}



#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lexer_initialization() {
        let mut lexer = Lexer::from("a");

        assert_eq!(lexer.input, vec!(b'a'));
        assert_eq!(lexer.ch, '\0');
        assert_eq!(lexer.position, 0);
        assert_eq!(lexer.read_position, 0);

        lexer.read_char();

        assert_eq!(lexer.input, vec!(b'a'));
        assert_eq!(lexer.ch, 'a');
        assert_eq!(lexer.position, 0);
        assert_eq!(lexer.read_position, 1);
    }

    #[test]
    fn test_base_tokens() {
        let source = "=+(){},;";
        let mut lexer = Lexer::from(source);


        assert_eq!(lexer.next_token(), Token::ASSIGN);
        assert_eq!(lexer.next_token(), Token::PLUS);
        assert_eq!(lexer.next_token(), Token::LPAREN);
        assert_eq!(lexer.next_token(), Token::RPAREN);
        assert_eq!(lexer.next_token(), Token::LBRACE);
        assert_eq!(lexer.next_token(), Token::RBRACE);
        assert_eq!(lexer.next_token(), Token::COMMA);
        assert_eq!(lexer.next_token(), Token::SEMICOLON);
        assert_eq!(lexer.next_token(), Token::EOF);
    }

    #[test]
    fn test_all_tokens() {
        let source = "let five = 5;
                            let ten = 10;
                            let add = fn(x, y) { x + y; };
                            let result = add(five, ten);";

        let mut lexer = Lexer::from(source);

        assert_eq!(lexer.next_token(), Token::LET);
        assert_eq!(lexer.next_token(), Token::IDENT { value: "five".to_owned() });
        assert_eq!(lexer.next_token(), Token::ASSIGN);
        assert_eq!(lexer.next_token(), Token::INT { value: 5 });
        assert_eq!(lexer.next_token(), Token::SEMICOLON);

        assert_eq!(lexer.next_token(), Token::LET);
        assert_eq!(lexer.next_token(), Token::IDENT { value: "ten".to_owned() });
        assert_eq!(lexer.next_token(), Token::ASSIGN);
        assert_eq!(lexer.next_token(), Token::INT { value: 10 });
        assert_eq!(lexer.next_token(), Token::SEMICOLON);

        assert_eq!(lexer.next_token(), Token::LET);
        assert_eq!(lexer.next_token(), Token::IDENT { value: "add".to_owned() });
        assert_eq!(lexer.next_token(), Token::ASSIGN);
        assert_eq!(lexer.next_token(), Token::FUNCTION);
        assert_eq!(lexer.next_token(), Token::LPAREN);
        assert_eq!(lexer.next_token(), Token::IDENT { value: "x".to_owned() });
        assert_eq!(lexer.next_token(), Token::COMMA);
        assert_eq!(lexer.next_token(), Token::IDENT { value: "y".to_owned() });
        assert_eq!(lexer.next_token(), Token::RPAREN);
        {
            assert_eq!(lexer.next_token(), Token::LBRACE);

            assert_eq!(lexer.next_token(), Token::IDENT { value: "x".to_owned() });
            assert_eq!(lexer.next_token(), Token::PLUS);
            assert_eq!(lexer.next_token(), Token::IDENT { value: "y".to_owned() });
            assert_eq!(lexer.next_token(), Token::SEMICOLON);

            assert_eq!(lexer.next_token(), Token::RBRACE);
            assert_eq!(lexer.next_token(), Token::SEMICOLON);
        }


        assert_eq!(lexer.next_token(), Token::LET);
        assert_eq!(lexer.next_token(), Token::IDENT { value: "result".to_owned() });
        assert_eq!(lexer.next_token(), Token::ASSIGN);
        assert_eq!(lexer.next_token(), Token::IDENT { value: "add".to_owned() });
        assert_eq!(lexer.next_token(), Token::LPAREN);
        assert_eq!(lexer.next_token(), Token::IDENT { value: "five".to_owned() });
        assert_eq!(lexer.next_token(), Token::COMMA);
        assert_eq!(lexer.next_token(), Token::IDENT { value: "ten".to_owned() });
        assert_eq!(lexer.next_token(), Token::RPAREN);
        assert_eq!(lexer.next_token(), Token::SEMICOLON);

        assert_eq!(lexer.next_token(), Token::EOF);
    }
}