#[derive(Debug, PartialEq)]
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
}

pub struct Lexer {
    input: Vec<u8>,
    ch: u8,
    position: usize,
    read_position: usize,
}

impl From<&str> for Lexer {
    fn from(value: &str) -> Self {
        return Lexer {
            input: value.as_bytes().to_owned(), // todo
            ch: 0,
            position: 0,
            read_position: 0,
        };
    }
}

fn ascii_char(c: u8) -> String {
    String::from_utf8(vec!(c)).unwrap() // todo
}

impl Lexer {
    fn read_char(&mut self) {
        self.ch = if self.read_position >= self.input.len() {
            0
        } else {
            self.input[self.read_position]
        };

        self.position = self.read_position;
        self.read_position += 1;
    }

    fn next_token(&mut self) -> Token {
        self.read_char();
        let token = match self.ch {
            b'=' => Token::ASSIGN,
            b'+' => Token::PLUS,

            b',' => Token::COMMA,
            b';' => Token::SEMICOLON,

            b'(' => Token::LPAREN,
            b')' => Token::RPAREN,

            b'{' => Token::LBRACE,
            b'}' => Token::RBRACE,

            0 => Token::EOF,
            c => Token::ILLEGAL { value: ascii_char(c) }
        };
        return token;
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lexer_initialization() {
        let mut lexer = Lexer::from("a");

        assert_eq!(lexer.input, vec!(b'a'));
        assert_eq!(lexer.ch, 0);
        assert_eq!(lexer.position, 0);
        assert_eq!(lexer.read_position, 0);

        lexer.read_char();

        assert_eq!(lexer.input, vec!(b'a'));
        assert_eq!(lexer.ch, b'a');
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
}