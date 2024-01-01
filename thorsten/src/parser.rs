use std::fmt::Binary;

use crate::lexer::{Identifier, Lexer, Token};
use crate::lexer::Token::{Eof, Semicolon};
use crate::parser::Expression::{ExprError, ExprPrefix};
use crate::parser::Node::{StmtError, StmtExpr, StmtLet, StmtReturn};
use crate::parser::Precedence::{Lowest, Prefix};
use crate::parser::PrefixKind::{Minus, Not};

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

impl From<&Token> for Precedence {
    fn from(value: &Token) -> Self {
        match value {
            Token::Plus => Precedence::Sum,
            Token::Minus => Precedence::Sum,
            Token::Asterisk => Precedence::Product,
            Token::Slash => Precedence::Product,
            Token::Lt => Precedence::LesserGreater,
            Token::Gt => Precedence::LesserGreater,
            Token::Equals => Precedence::Equals,
            Token::Differs => Precedence::Equals,
            _ => Precedence::Lowest,
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Node {
    StmtError { current: Token, peek: Token },
    StmtLet { name: Identifier, value: Expression },
    StmtReturn { value: Expression },
    StmtExpr { value: Expression },
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Expression {
    ExprInteger {
        value: i32,
    },
    ExprIdent {
        name: Identifier,
    },
    ExprPrefix {
        kind: PrefixKind,
        value: Box<Expression>,
    },
    ExprBinary {
        left: Box<Expression>,
        right: Box<Expression>,
        kind: ExprKind,
    },
    ExprError {
        value: Vec<Token>,
    },
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum PrefixKind {
    Not,
    Minus,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum ExprKind {
    Plus,
    Minus,
    Times,
    Div,
    Greater,
    Lesser,
    Equals,
    Differs,
}

impl TryFrom<&Token> for ExprKind {
    type Error = ();
    fn try_from(value: &Token) -> Result<Self, Self::Error> {
        match *value {
            Token::Plus => Ok(ExprKind::Plus),
            Token::Minus => Ok(ExprKind::Minus),
            Token::Slash => Ok(ExprKind::Div),
            Token::Asterisk => Ok(ExprKind::Times),
            Token::Equals => Ok(ExprKind::Equals),
            Token::Differs => Ok(ExprKind::Differs),
            Token::Lt => Ok(ExprKind::Lesser),
            Token::Gt => Ok(ExprKind::Greater),
            _ => Err(()),
        }
    }
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
    fn next_token(&mut self) -> Token {
        if self.current == Eof && self.peek == Eof {
            self.current = self.lexer.next_token();
            self.peek = self.lexer.next_token();
        } else {
            self.current = self.peek.clone(); // todo
            self.peek = self.lexer.next_token();
        }
        return self.current.clone();
    }

    fn next_statement(&mut self) -> Node {
        self.next_token();
        let res = match self.current.clone() {
            Token::Return => {
                self.next_token();
                StmtReturn {
                    value: self.next_prefix(Lowest),
                }
            }

            Token::Let => {
                let ident = self.next_token();
                let equals = self.next_token();
                let expr = self.next_token();
                match (ident, equals, expr) {
                    (Token::Ident { name }, Token::Assign, _) => StmtLet {
                        name,
                        value: self.next_prefix(Lowest),
                    },
                    (ident, assign, expr) => StmtError { current: ident, peek: assign }
                }
            }

            _ => StmtExpr { value: self.next_prefix(Lowest) }

        };

        if self.peek == Token::Semicolon {
            self.next_token();
        }
        return res;
    }

    fn next_prefix(&mut self, precedence: Precedence) -> Expression {
        let mut left = match self.current.clone() {
            Token::Ident { name } => Expression::ExprIdent { name },
            Token::Int { value } => Expression::ExprInteger { value },
            Token::Bang => {
                self.next_token();
                ExprPrefix {
                    kind: Not,
                    value: self.next_prefix(Prefix).into(),
                }
            }
            Token::Minus => {
                self.next_token();
                ExprPrefix {
                    kind: Minus,
                    value: self.next_prefix(Prefix).into(),
                }
            }
            _ => ExprError {
                value: vec![self.current.clone()],
            },
        };

        while self.peek != Semicolon && precedence < Precedence::from(&self.peek) {
            left = self.next_infix(left)
        }

        return left;
    }

    fn next_infix(&mut self, left: Expression) -> Expression {
        match ExprKind::try_from(&self.peek) {
            Ok(kind) => {
                let precedence = Precedence::from(&self.peek);
                self.next_token();
                self.next_token();
                Expression::ExprBinary {
                    left: left.into(),
                    right: self.next_prefix(precedence).into(),
                    kind,
                }
            }
            _ => left,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::Expression::{ExprBinary, ExprIdent, ExprInteger, ExprPrefix};
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
                value: ExprIdent {
                    name: Identifier::from("name")
                }
            },
        );
        assert_eq!(
            parser.next_statement(),
            StmtExpr {
                value: ExprIdent {
                    name: Identifier::from("name")
                }
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
                value: ExprPrefix {
                    kind: PrefixKind::Not,
                    value: Box::from(ExprInteger { value: 5 })
                }
            },
        );
        assert_eq!(
            parser.next_statement(),
            StmtExpr {
                value: ExprPrefix {
                    kind: PrefixKind::Minus,
                    value: Box::from(ExprInteger { value: 15 })
                }
            },
        );
    }

    #[test]
    fn infix_operators() {
        let input = "
        5 + 5;
        5 - 5;
        5 * 5;
        5 / 5;
        5 > 5;
        5 < 5;
        5 == 5;
        5 != 5;
        ";

        let mut parser = Parser::from(input);

        assert_eq!(
            parser.next_statement(),
            StmtExpr {
                value: ExprBinary {
                    kind: ExprKind::Plus,
                    left: ExprInteger { value: 5 }.into(),
                    right: ExprInteger { value: 5 }.into()
                }
            },
        );

        assert_eq!(
            parser.next_statement(),
            StmtExpr {
                value: ExprBinary {
                    kind: ExprKind::Minus,
                    left: ExprInteger { value: 5 }.into(),
                    right: ExprInteger { value: 5 }.into()
                }
            },
        );

        assert_eq!(
            parser.next_statement(),
            StmtExpr {
                value: ExprBinary {
                    kind: ExprKind::Times,
                    left: ExprInteger { value: 5 }.into(),
                    right: ExprInteger { value: 5 }.into()
                }
            },
        );

        assert_eq!(
            parser.next_statement(),
            StmtExpr {
                value: ExprBinary {
                    kind: ExprKind::Div,
                    left: ExprInteger { value: 5 }.into(),
                    right: ExprInteger { value: 5 }.into()
                }
            },
        );

        assert_eq!(
            parser.next_statement(),
            StmtExpr {
                value: ExprBinary {
                    kind: ExprKind::Greater,
                    left: ExprInteger { value: 5 }.into(),
                    right: ExprInteger { value: 5 }.into()
                }
            },
        );

        assert_eq!(
            parser.next_statement(),
            StmtExpr {
                value: ExprBinary {
                    kind: ExprKind::Lesser,
                    left: ExprInteger { value: 5 }.into(),
                    right: ExprInteger { value: 5 }.into()
                }
            },
        );

        assert_eq!(
            parser.next_statement(),
            StmtExpr {
                value: ExprBinary {
                    kind: ExprKind::Equals,
                    left: ExprInteger { value: 5 }.into(),
                    right: ExprInteger { value: 5 }.into()
                }
            },
        );

        assert_eq!(
            parser.next_statement(),
            StmtExpr {
                value: ExprBinary {
                    kind: ExprKind::Differs,
                    left: ExprInteger { value: 5 }.into(),
                    right: ExprInteger { value: 5 }.into()
                }
            },
        );
    }

    #[test]
    fn infix_precendece() {
        let input = "
        -a * b;
        !-a;
        a + b + c;
        a + b - c;
        a * b * c;
        a * b / c;
        a + b / c;
        a + b * c + d / e - f;
        3 + 4; -5 * 5;
        5 > 4 == 3 < 4;
        5 < 4 != 3 > 4;
        3 + 4 * 5 == 3 * 1 + 4 * 5;
        3 + 4 * 5 == 3 * 1 + 4 * 5;
        ";

        let mut parser = Parser::from(input);

        assert_eq!(
            parser.next_statement(),
            StmtExpr {
                value: ExprBinary {
                    left: ExprPrefix {
                        kind: PrefixKind::Minus,
                        value: ExprIdent {
                            name: Identifier::from("a")
                        }
                            .into(),
                    }
                        .into(),
                    kind: ExprKind::Times,
                    right: ExprIdent {
                        name: Identifier::from("b")
                    }
                        .into(),
                }
            }
        );

        assert_eq!(
            parser.next_statement(),
            StmtExpr {
                value: ExprPrefix {
                    kind: PrefixKind::Not,
                    value: ExprPrefix {
                        kind: PrefixKind::Minus,
                        value: ExprIdent {
                            name: Identifier::from("a")
                        }
                            .into(),
                    }
                        .into(),
                }
            }
        );
    }
}
