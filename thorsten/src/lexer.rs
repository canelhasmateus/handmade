use crate::lexer::LiteralKind::{DIGIT, EQ, LETTER, OTHER, WHITESPACE};

#[derive(Debug, Eq, PartialEq)]
pub enum Token {
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

    Illegal { value: String },
    Ident { value: String },
    Int { value: i32 },
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
    input: String,
    ch: char,
    position: usize,
    read_position: usize,
}

impl From<&str> for Lexer {
    fn from(value: &str) -> Self {
        return Lexer {
            input: value.to_owned(), // todo
            ch: '\0',
            position: 0,
            read_position: 0,
        };
    }
}

impl Lexer {
    pub fn next_token(&mut self) -> Token {
        loop {
            let token = self.next_real_token();
            if let Token::Blank { value: _ } = token {
                continue
            } else {
                return token
            }
        }
    }
    fn read_char(&mut self) -> char {
        self.ch = if self.read_position >= self.input.len() {
            '\0'
        } else {
            self.input.as_bytes()[self.read_position] as char
        };

        self.position = self.read_position;
        self.read_position += 1;

        return self.ch
    }
    fn next_real_token(&mut self) -> Token {
        return match self.read_char() {
            '*' => Token::Asterisk,
            '/' => Token::Slash,
            '+' => Token::Plus,
            '-' => Token::Minus,

            ',' => Token::Comma,
            ';' => Token::Semicolon,

            '(' => Token::Lparen,
            ')' => Token::Rparen,

            '{' => Token::Lbrace,
            '}' => Token::Rbrace,
            '<' => Token::Lt,
            '>' => Token::Gt,

            '\0' => Token::Eof,

            c => match LiteralKind::from(c) {
                DIGIT => Token::Int { value: self.span(DIGIT).parse::<i32>().unwrap() },

                WHITESPACE => Token::Blank { value: self.span(WHITESPACE).into() },

                OTHER => Token::Illegal { value: self.span(OTHER).into() },

                EQ => match self.span(EQ) {
                    "=" => Token::Assign,
                    "!" => Token::Bang,
                    "==" => Token::Equals,
                    "!=" => Token::Differs,
                    s => Token::Illegal { value: s.into() }
                }

                LETTER => match self.span(LETTER) {
                    "let" => Token::Let,
                    "fn" => Token::Function,
                    "true" => Token::True,
                    "false" => Token::False,
                    "if" => Token::If,
                    "else" => Token::Else,
                    "return" => Token::Return,
                    s => Token::Ident { value: s.into() }
                }
            }
        };
    }

    fn span(&mut self, kind: LiteralKind) -> &str {
        let initial_pos = self.position;
        while LiteralKind::from(self.peek()) == kind {
            self.read_char();
        }

        return &self.input[initial_pos..=self.position];
    }

    fn peek(&mut self) -> char {
        let idx = self.position + 1;
        if idx >= self.input.len() {
            return '\0';
        }
        return self.input.as_bytes()[idx] as char;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lexer_initialization() {
        let mut lexer = Lexer::from("a");

        assert_eq!(lexer.input, "a");
        assert_eq!(lexer.ch, '\0');
        assert_eq!(lexer.position, 0);
        assert_eq!(lexer.read_position, 0);

        lexer.read_char();

        assert_eq!(lexer.input, "a");
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


        assert_eq!(lexer.next_token(), Token::Assign);
        assert_eq!(lexer.next_token(), Token::Plus);
        assert_eq!(lexer.next_token(), Token::Lparen);
        assert_eq!(lexer.next_token(), Token::Rparen);
        assert_eq!(lexer.next_token(), Token::Lbrace);
        assert_eq!(lexer.next_token(), Token::Rbrace);
        assert_eq!(lexer.next_token(), Token::Comma);
        assert_eq!(lexer.next_token(), Token::Semicolon);
        assert_eq!(lexer.next_token(), Token::Eof);
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

        assert_eq!(lexer.next_token(), Token::Let);
        assert_eq!(lexer.next_token(), Token::Ident { value: "five".to_owned() });
        assert_eq!(lexer.next_token(), Token::Assign);
        assert_eq!(lexer.next_token(), Token::Int { value: 5 });
        assert_eq!(lexer.next_token(), Token::Semicolon);

        assert_eq!(lexer.next_token(), Token::Let);
        assert_eq!(lexer.next_token(), Token::Ident { value: "ten".to_owned() });
        assert_eq!(lexer.next_token(), Token::Assign);
        assert_eq!(lexer.next_token(), Token::Int { value: 10 });
        assert_eq!(lexer.next_token(), Token::Semicolon);

        assert_eq!(lexer.next_token(), Token::Let);
        assert_eq!(lexer.next_token(), Token::Ident { value: "add".to_owned() });
        assert_eq!(lexer.next_token(), Token::Assign);
        assert_eq!(lexer.next_token(), Token::Function);
        assert_eq!(lexer.next_token(), Token::Lparen);
        assert_eq!(lexer.next_token(), Token::Ident { value: "x".to_owned() });
        assert_eq!(lexer.next_token(), Token::Comma);
        assert_eq!(lexer.next_token(), Token::Ident { value: "y".to_owned() });
        assert_eq!(lexer.next_token(), Token::Rparen);
        {
            assert_eq!(lexer.next_token(), Token::Lbrace);

            assert_eq!(lexer.next_token(), Token::Ident { value: "x".to_owned() });
            assert_eq!(lexer.next_token(), Token::Plus);
            assert_eq!(lexer.next_token(), Token::Ident { value: "y".to_owned() });
            assert_eq!(lexer.next_token(), Token::Semicolon);

            assert_eq!(lexer.next_token(), Token::Rbrace);
            assert_eq!(lexer.next_token(), Token::Semicolon);
        }


        assert_eq!(lexer.next_token(), Token::Let);
        assert_eq!(lexer.next_token(), Token::Ident { value: "result".to_owned() });
        assert_eq!(lexer.next_token(), Token::Assign);
        assert_eq!(lexer.next_token(), Token::Ident { value: "add".to_owned() });
        assert_eq!(lexer.next_token(), Token::Lparen);
        assert_eq!(lexer.next_token(), Token::Ident { value: "five".to_owned() });
        assert_eq!(lexer.next_token(), Token::Comma);
        assert_eq!(lexer.next_token(), Token::Ident { value: "ten".to_owned() });
        assert_eq!(lexer.next_token(), Token::Rparen);
        assert_eq!(lexer.next_token(), Token::Semicolon);
        assert_eq!(lexer.next_token(), Token::Eof);
    }

    #[test]
    fn test_all_tokens_2() {
        let source = "
        !-/*5;
        5 < 10 > 5;
        ";

        let mut lexer = Lexer::from(source);

        assert_eq!(lexer.next_token(), Token::Bang);
        assert_eq!(lexer.next_token(), Token::Minus);
        assert_eq!(lexer.next_token(), Token::Slash);
        assert_eq!(lexer.next_token(), Token::Asterisk);
        assert_eq!(lexer.next_token(), Token::Int { value: 5 });
        assert_eq!(lexer.next_token(), Token::Semicolon);

        assert_eq!(lexer.next_token(), Token::Int { value: 5 });
        assert_eq!(lexer.next_token(), Token::Lt);
        assert_eq!(lexer.next_token(), Token::Int { value: 10 });
        assert_eq!(lexer.next_token(), Token::Gt);
        assert_eq!(lexer.next_token(), Token::Int { value: 5 });
        assert_eq!(lexer.next_token(), Token::Semicolon);

        assert_eq!(lexer.next_token(), Token::Eof);
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

        assert_eq!(lexer.next_token(), Token::If);
        assert_eq!(lexer.next_token(), Token::Lparen);
        assert_eq!(lexer.next_token(), Token::Int { value: 5 });
        assert_eq!(lexer.next_token(), Token::Lt);
        assert_eq!(lexer.next_token(), Token::Int { value: 10 });
        assert_eq!(lexer.next_token(), Token::Rparen);
        assert_eq!(lexer.next_token(), Token::Lbrace);
        {
            assert_eq!(lexer.next_token(), Token::Return);
            assert_eq!(lexer.next_token(), Token::True);
            assert_eq!(lexer.next_token(), Token::Semicolon);
        }
        assert_eq!(lexer.next_token(), Token::Rbrace);
        assert_eq!(lexer.next_token(), Token::Else);
        assert_eq!(lexer.next_token(), Token::Lbrace);
        {
            assert_eq!(lexer.next_token(), Token::Return);
            assert_eq!(lexer.next_token(), Token::False);
            assert_eq!(lexer.next_token(), Token::Semicolon);
        }
        assert_eq!(lexer.next_token(), Token::Rbrace);
        assert_eq!(lexer.next_token(), Token::Eof);
    }

    #[test]
    fn test_all_tokens_4() {
        let source = "
        10 == 10;
        10 != 9;
        ";

        let mut lexer = Lexer::from(source);

        assert_eq!(lexer.next_token(), Token::Int { value: 10 });
        assert_eq!(lexer.next_token(), Token::Equals);
        assert_eq!(lexer.next_token(), Token::Int { value: 10 });
        assert_eq!(lexer.next_token(), Token::Semicolon);

        assert_eq!(lexer.next_token(), Token::Int { value: 10 });
        assert_eq!(lexer.next_token(), Token::Differs);
        assert_eq!(lexer.next_token(), Token::Int { value: 9 });
        assert_eq!(lexer.next_token(), Token::Semicolon);

        assert_eq!(lexer.next_token(), Token::Eof);
    }
}