use crate::lexer::{Identifier, Lexer, Token};
use crate::parser::Expression::Error;
use crate::parser::Node::{ErrorNode, Let, Return, Root};

#[derive(Debug, Eq, PartialEq, Clone)]
struct Program {
    statements: Vec<Node>,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Node {
    ErrorNode { current: Token, peek: Token },
    Root { program: Program },
    Let { name: Identifier, value: Expression },
    Return { value: Expression },
}
#[derive(Debug, Eq, PartialEq, Clone)]
enum Expression {
    Integer { value: i32 },
    Error { value: Vec<Token> },
}

struct Parser {
    lexer: Lexer,
    current: Token,
    peek: Token,
}

impl Parser {
    fn program(&mut self) -> Node {
        let mut statements = vec![];

        loop {
            if self.current == Token::Eof {
                break;
            }

            statements.push(self.next_statement())
        }

        return Root {
            program: Program { statements },
        };
    }
    fn next_token(&mut self) {
        self.current = self.peek.clone(); // todo
        self.peek = self.lexer.next_token();
    }
    fn next_statement(&mut self) -> Node {
        match (self.current.clone(), self.peek.clone()) {
            (Token::Return, _) => {
                while self.current != Token::Semicolon {
                    self.next_token();
                }
                self.next_token();
                Return { value: Error { value: vec!() } }
            },
            (Token::Let, Token::Ident { value: name }) => {
                while self.current != Token::Semicolon {
                    self.next_token();
                }
                self.next_token();

                let mut value: Vec<Token> = vec![];
                Let {
                    name,
                    value: Error { value },
                }
            }

            (current, peek) => {
                self.next_token();
                ErrorNode { current, peek }
            }
        }
    }
}

impl From<&str> for Parser {
    fn from(value: &str) -> Self {
        let mut lexer = Lexer::from(value);
        let current = lexer.next_token();
        let peek = lexer.next_token();
        return Parser {
            lexer,
            current,
            peek,
        };
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::Node::Let;

    use super::*;

    #[test]
    fn parser_initialization() {
        let mut parser = Parser::from("1 + 2");

        assert_eq!(parser.current, Token::Int { value: 1 });
        assert_eq!(parser.peek, Token::Plus);

        parser.next_token();

        assert_eq!(parser.current, Token::Plus);
        assert_eq!(parser.peek, Token::Int { value: 2 });

        parser.next_token();

        assert_eq!(parser.current, Token::Int { value: 2 });
        assert_eq!(parser.peek, Token::Eof);
    }

    #[test]
    fn let_statements() {
        let input = "
        let x = 5;
        let y = 10;
        let foobar = 838383;
        ";

        let mut parser = Parser::from(input);

        assert_eq!(
            parser.next_statement(),
            Let {
                name: Identifier::from("x"),
                value: Error { value: vec!() }
            },
        );
        assert_eq!(
            parser.next_statement(),
            Let {
                name: Identifier::from("y"),
                value: Error { value: vec!() }
            },
        );
        assert_eq!(
            parser.next_statement(),
            Let {
                name: Identifier::from("foobar"),
                value: Error { value: vec!() },
            }
        );
    }

    #[test]
    fn return_statements() {
        let input = "
        return 5;
        return 10;
        return 993322;
        ";

        let mut parser = Parser::from(input);

        assert_eq!(
            parser.next_statement(),
            Return {
                value: Error { value: vec!()}
            },
        );
        assert_eq!(
            parser.next_statement(),
            Return {
                value: Error { value: vec!()}
            },
        );
        assert_eq!(
            parser.next_statement(),
            Return {
                value: Error { value: vec!()}
            },
        );
    }
}
