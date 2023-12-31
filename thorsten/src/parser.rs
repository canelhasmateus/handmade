use crate::lexer::{Identifier, Lexer, Token};
use crate::parser::Node::Root;

#[derive(Debug, Eq, PartialEq, Clone)]
struct Program {
    statements: Vec<Node>,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Node {
    Root { value: Program },
    Let {
        name: Identifier,
        value: Expression,
    }

}

trait Statement {}

#[derive(Debug, Eq, PartialEq, Clone)]
enum Expression {
    Integer { value: i32 }
}

struct Parser {
    lexer: Lexer,
    current: Token,
    peek: Token,
}


impl Parser {
    fn program(&mut self) -> Node {
        return Root {
            value: Program {
                statements: vec!()
            }
        }
    }
    fn next_token(&mut self) {
        self.current = self.peek.clone(); // todo
        self.peek = self.lexer.next_token();
    }
}

impl From<&str> for Parser {
    fn from(value: &str) -> Self {
        let mut lexer = Lexer::from(value);
        let current = lexer.next_token();
        let peek = lexer.next_token();
        return Parser { lexer, current, peek }
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::Expression::Integer;
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
        assert_eq!(parser.program(), Root {
            value: Program {
                statements: vec!(
                    Let { name: Identifier::from("x"), value: Integer { value: 0 } },
                    Let { name: Identifier::from("y"), value: Integer { value: 10 } },
                    Let { name: Identifier::from("foobar"), value: Integer { value: 838383 } }
                )
            }
        });
    }
}
