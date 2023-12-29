use crate::lexer::LiteralKind::{DIGIT, EQ, LETTER, OTHER, WHITESPACE};

fn from_ascii_vec(name: Vec<u8>) -> String {
    return String::from_utf8(name).unwrap(); // todo
}

#[derive(Debug, Eq, PartialEq)]
enum Token {
    EOF,
    BANG,
    ASSIGN,
    COMMA,
    SEMICOLON,

    PLUS,
    MINUS,
    ASTERISK,
    SLASH,

    LT,
    GT,

    LPAREN,
    RPAREN,

    LBRACE,
    RBRACE,

    EQUALS,
    DIFFERS,

    FUNCTION,
    LET,
    TRUE,
    FALSE,
    IF,
    ELSE,
    RETURN,

    ILLEGAL { value: String },
    IDENT { value: String },
    INT { value: i32 },
    WHITESPACE { value: String },
}

#[derive(Eq, PartialEq)]
enum LiteralKind {
    EQ,
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
            '=' => EQ,
            '!' => EQ,
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
            '*' => Token::ASTERISK,
            '/' => Token::SLASH,
            '+' => Token::PLUS,
            '-' => Token::MINUS,

            ',' => Token::COMMA,
            ';' => Token::SEMICOLON,

            '(' => Token::LPAREN,
            ')' => Token::RPAREN,

            '{' => Token::LBRACE,
            '}' => Token::RBRACE,
            '<' => Token::LT,
            '>' => Token::GT,

            '\0' => Token::EOF,

            '!' => match self.peek() {
                '=' => {
                    self.read_char();
                    Token::DIFFERS
                },
                _ => Token::BANG
            }

            '=' => match self.peek() {
                '=' => {
                    self.read_char();
                    Token::EQUALS
                },
                _ => Token::ASSIGN
            }

            c => match LiteralKind::from(c) {

                DIGIT => Token::INT { value: from_ascii_vec(self.agglomerate(DIGIT)).parse::<i32>().unwrap() },

                WHITESPACE => Token::WHITESPACE { value: from_ascii_vec(self.agglomerate(WHITESPACE)) },

                OTHER => Token::ILLEGAL { value: from_ascii_vec(self.agglomerate(OTHER)) },

                EQ => {
                    let span = from_ascii_vec(self.agglomerate(EQ));
                    match span.as_str() {
                        "=" => Token::ASSIGN,
                        "!" => Token::BANG,
                        "==" => Token::EQUALS,
                        "!=" => Token::DIFFERS,
                        _ => Token::ILLEGAL { value: span }
                    }
                }

                LETTER => {
                    let name = from_ascii_vec(self.agglomerate(LETTER));
                    match name.as_str() {
                        "let" => Token::LET,
                        "fn" => Token::FUNCTION,
                        "true" => Token::TRUE,
                        "false" => Token::FALSE,
                        "if" => Token::IF,
                        "else" => Token::ELSE,
                        "return" => Token::RETURN,
                        _ => Token::IDENT { value: name }
                    }
                }
            }
        };
    }

    fn agglomerate(&mut self, kind: LiteralKind) -> Vec<u8> {
        let initial_pos = self.position;
        while LiteralKind::from(self.peek()) == kind {
            self.read_char();
        }

        return self.input[initial_pos..=self.position].to_vec();
    }

    fn peek(&mut self) -> char {
        let idx = self.position + 1;
        if idx >= self.input.len() {
            return '\0';
        }
        return self.input[idx] as char;
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
        let source = "
        =+(){},;
        ";
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
    fn test_all_tokens_1() {
        let source = "
        let five = 5;
        let ten = 10;
        let add = fn(x, y) { x + y; };
        let result = add(five, ten);
        ";

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

    #[test]
    fn test_all_tokens_2() {
        let source = "
        !-/*5;
        5 < 10 > 5;
        ";

        let mut lexer = Lexer::from(source);

        assert_eq!(lexer.next_token(), Token::BANG);
        assert_eq!(lexer.next_token(), Token::MINUS);
        assert_eq!(lexer.next_token(), Token::SLASH);
        assert_eq!(lexer.next_token(), Token::ASTERISK);
        assert_eq!(lexer.next_token(), Token::INT { value: 5 });
        assert_eq!(lexer.next_token(), Token::SEMICOLON);

        assert_eq!(lexer.next_token(), Token::INT { value: 5 });
        assert_eq!(lexer.next_token(), Token::LT);
        assert_eq!(lexer.next_token(), Token::INT { value: 10 });
        assert_eq!(lexer.next_token(), Token::GT);
        assert_eq!(lexer.next_token(), Token::INT { value: 5 });
        assert_eq!(lexer.next_token(), Token::SEMICOLON);

        assert_eq!(lexer.next_token(), Token::EOF);
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

        assert_eq!(lexer.next_token(), Token::IF);
        assert_eq!(lexer.next_token(), Token::LPAREN);
        assert_eq!(lexer.next_token(), Token::INT { value: 5 });
        assert_eq!(lexer.next_token(), Token::LT);
        assert_eq!(lexer.next_token(), Token::INT { value: 10 });
        assert_eq!(lexer.next_token(), Token::RPAREN);
        assert_eq!(lexer.next_token(), Token::LBRACE);
        {
            assert_eq!(lexer.next_token(), Token::RETURN);
            assert_eq!(lexer.next_token(), Token::TRUE);
            assert_eq!(lexer.next_token(), Token::SEMICOLON);
        }
        assert_eq!(lexer.next_token(), Token::RBRACE);
        assert_eq!(lexer.next_token(), Token::ELSE);
        assert_eq!(lexer.next_token(), Token::LBRACE);
        {
            assert_eq!(lexer.next_token(), Token::RETURN);
            assert_eq!(lexer.next_token(), Token::FALSE);
            assert_eq!(lexer.next_token(), Token::SEMICOLON);
        }
        assert_eq!(lexer.next_token(), Token::RBRACE);
        assert_eq!(lexer.next_token(), Token::EOF);
    }

    #[test]
    fn test_all_tokens_4() {
        let source = "
        10 == 10;
        10 != 9;
        ";

        let mut lexer = Lexer::from(source);

        assert_eq!(lexer.next_token(), Token::INT { value: 10 });
        assert_eq!(lexer.next_token(), Token::EQUALS);
        assert_eq!(lexer.next_token(), Token::INT { value: 10 });
        assert_eq!(lexer.next_token(), Token::SEMICOLON);

        assert_eq!(lexer.next_token(), Token::INT { value: 10 });
        assert_eq!(lexer.next_token(), Token::DIFFERS);
        assert_eq!(lexer.next_token(), Token::INT { value: 9 });
        assert_eq!(lexer.next_token(), Token::SEMICOLON);

        assert_eq!(lexer.next_token(), Token::EOF);
    }
}