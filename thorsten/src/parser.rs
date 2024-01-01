use crate::lexer::{Identifier, Lexer, Token};
use crate::lexer::Token::Eof;
use crate::parser::Expression::{ExprError, ExprPrefix};
use crate::parser::Node::{StmtError, StmtExpr, StmtLet, StmtReturn};
use crate::parser::Precedence::{Lowest, Prefix};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
enum Precedence {
    Lowest,
    Equals,
    LesserGreater,
    Sum,
    Product,
    Prefix,
    Call,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Node {
    StmtError { current: Token, peek: Token },
    StmtLet { name: Identifier, value: Expression },
    StmtReturn { value: Expression },
    StmtExpr { value: Expression }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Expression {
    ExprInteger { value: i32 },
    ExprIdent { name: Identifier },
    ExprPrefix { kind: PrefixKind, value: Box<Expression> },
    ExprError { value: Vec<Token> },
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum PrefixKind {
    Not,
    Minus,
}

struct Parser {
    lexer: Lexer,
    current: Token,
    peek: Token,
}

impl From<&str> for Parser {
    fn from(value: &str) -> Self {
        return Parser {
            lexer: Lexer::from(value),
            current: Token::Eof,
            peek: Token::Eof,
        };
    }
}

impl Parser {
    fn next_token(&mut self) {
        if self.current == Eof && self.peek == Eof {
            self.current = self.lexer.next_token();
            self.peek = self.lexer.next_token();
        } else {
            self.current = self.peek.clone(); // todo
            self.peek = self.lexer.next_token();
        }
    }

    fn next_statement(&mut self) -> Node {
        self.next_token();
        return match (self.current.clone(), self.peek.clone()) { // todo
            (Token::Minus, _) => {
                self.next_token();
                StmtExpr {
                    value: ExprPrefix {
                        kind: PrefixKind::Minus,
                        value: Box::from(self.next_expression(Prefix)),
                    }
                }
            },

            (Token::Bang, _) => {
                self.next_token();
                StmtExpr {
                    value: ExprPrefix {
                        kind: PrefixKind::Not,
                        value: Box::from(self.next_expression(Prefix)),
                    }
                }
            },

            (Token::Ident { .. }, _) => StmtExpr { value: self.next_expression(Lowest) },

            (Token::Int { .. }, _) => StmtExpr { value: self.next_expression(Lowest) },

            (Token::Return, _) => {
                self.next_token();
                StmtReturn { value: self.next_expression(Lowest) }
            },

            (Token::Let, Token::Ident { name }) => {
                self.next_token();
                self.next_token();
                match (self.current.clone(), self.peek.clone()) {
                    (Token::Assign, _) => {
                        self.next_token();
                        StmtLet { name, value: self.next_expression(Lowest) }
                    }
                    (current, peek) => StmtError { current, peek }
                }
            },

            (current, peek) => StmtError { current, peek }
        }
    }

    fn next_expression(&mut self, precedence: Precedence) -> Expression {
        return match (self.current.clone(), self.peek.clone()) { // todo
            (Token::Ident { name }, next) => {
                if next == Token::Semicolon {
                    self.next_token();
                }
                Expression::ExprIdent { name }
            },

            (Token::Int { value }, next) => {
                if next == Token::Semicolon {
                    self.next_token();
                }
                Expression::ExprInteger { value }
            },

            _ => ExprError { value: vec!() }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::Expression::{ExprIdent, ExprInteger, ExprPrefix};
    use crate::parser::Node::StmtLet;

    use super::*;

    #[test]
    fn parser_initialization() {
        let mut parser = Parser::from("1 + 2");

        assert_eq!(parser.current, Token::Eof);
        assert_eq!(parser.peek, Token::Eof);

        parser.next_token();
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
            StmtLet {
                name: Identifier::from("x"),
                value: ExprInteger { value: 5 }
            },
        );
        assert_eq!(
            parser.next_statement(),
            StmtLet {
                name: Identifier::from("y"),
                value: ExprInteger { value: 10 }
            },
        );
        assert_eq!(
            parser.next_statement(),
            StmtLet {
                name: Identifier::from("foobar"),
                value: ExprInteger { value: 838383 }
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
            StmtReturn {
                value: ExprInteger { value: 5 }
            },
        );
        assert_eq!(
            parser.next_statement(),
            StmtReturn {
                value: ExprInteger { value: 10 }
            },
        );
        assert_eq!(
            parser.next_statement(),
            StmtReturn {
                value: ExprInteger { value: 993322 }
            },
        );
    }

    #[test]
    fn expression_statements() {
        let input = "
        name;
        name
        5;
        5
        !5;
        -15;
        ";

        let mut parser = Parser::from(input);

        assert_eq!(
            parser.next_statement(),
            StmtExpr {
                value: ExprIdent { name: Identifier::from("name") }
            },
        );
        assert_eq!(
            parser.next_statement(),
            StmtExpr {
                value: ExprIdent { name: Identifier::from("name") }
            },
        );

        assert_eq!(
            parser.next_statement(),
            StmtExpr {
                value: ExprInteger { value: 5 }
            },
        );
        assert_eq!(
            parser.next_statement(),
            StmtExpr {
                value: ExprInteger { value: 5 }
            },
        );

        assert_eq!(
            parser.next_statement(),
            StmtExpr {
                value: ExprPrefix { kind: PrefixKind::Not , value: Box::from(ExprInteger { value: 5 }) }
            },
        );
        assert_eq!(
            parser.next_statement(),
            StmtExpr {
                value: ExprPrefix { kind: PrefixKind::Minus , value: Box::from(ExprInteger { value: 15 }) }
            },
        );
    }
}
