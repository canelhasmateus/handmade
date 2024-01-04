use ExpressionKind::{
    Binary, Identifier, IllegalExpression, LiteralBoolean, LiteralInteger, Unary,
};
use StatementKind::{IllegalStatement, ReturnStmt};
use TokenKind::{Assign, Bang, False, Ident, Int, Let, Lparen, Minus, Return, Rparen, True};

use crate::lexer::TokenKind::Semicolon;
use crate::lexer::{Lexer, Span, Token, TokenKind};
use crate::parser::ExpressionPrecedence::{Equals, LesserGreater, Lowest, Prefix, Product, Sum};

#[derive(Debug, PartialEq, Eq)]
pub struct Statement<'a> {
    pub kind: StatementKind<'a>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Expression<'a> {
    pub kind: ExpressionKind<'a>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq)]
pub enum StatementKind<'a> {
    LetStmt { name: &'a str, expr: Expression<'a> },
    ReturnStmt { expr: Expression<'a> },
    ExprStmt { expr: Expression<'a> },
    IllegalStatement { expr: Expression<'a> },
}

#[derive(Debug, PartialEq, Eq)]
pub enum ExpressionKind<'a> {
    LiteralInteger {
        value: i32,
    },
    LiteralBoolean {
        value: bool,
    },
    Identifier {
        name: &'a str,
    },
    Unary {
        op: UnaryOp,
        expr: Box<Expression<'a>>,
    },
    Binary {
        op: BinaryOp,
        left: Box<Expression<'a>>,
        right: Box<Expression<'a>>,
    },
    IllegalExpression {
        value: &'a str,
    },
}

#[derive(Debug, PartialEq, Eq)]
pub enum UnaryOp {
    Not,
    Minus,
}

#[derive(Debug, PartialEq, Eq)]
pub enum BinaryOp {
    Plus,
    Minus,
    Times,
    Div,
    Greater,
    Lesser,
    Equals,
    Differs,
}

#[derive(Debug, PartialEq, Eq)]
pub struct ConditionalExpression<'a> {
    condition: Box<Expression<'a>>,
    positive: Box<Expression<'a>>,
    negative: Option<Box<Expression<'a>>>,
}

#[derive(Clone, Copy, PartialOrd, PartialEq)]
pub enum ExpressionPrecedence {
    Lowest,
    Equals,
    LesserGreater,
    Sum,
    Product,
    Prefix,
}

pub struct Parser<'a> {
    lexer: Lexer<'a>,
}

impl Parser<'_> {
    pub fn statement_after(&self, span: &Span) -> Statement {
        let current = self.lexer.semantic_token_after(span);
        let mut statement = match current.kind {
            Return => {
                let expr = self.expression_after(&current.span, Lowest);
                Statement {
                    span: Span { start: current.span.start, end: expr.span.end },
                    kind: ReturnStmt { expr },
                }
            }

            Let => {
                let ident = self.lexer.semantic_token_after(&current.span);
                let equals = self.lexer.semantic_token_after(&ident.span);
                let expr = self.expression_after(&equals.span, Lowest);
                match (ident.kind, equals.kind) {
                    (Ident { name }, Assign) => Statement {
                        span: Span { start: current.span.start, end: expr.span.end },
                        kind: StatementKind::LetStmt { name, expr },
                    },
                    _ => Statement {
                        span: Span { start: current.span.start, end: expr.span.end },
                        kind: IllegalStatement { expr },
                    },
                }
            }

            _ => {
                let expr = self.expression_after(&span, Lowest);
                Statement {
                    span: Span { start: current.span.start, end: expr.span.end },
                    kind: StatementKind::ExprStmt { expr },
                }
            }
        };

        let mut after = self.lexer.semantic_token_after(&statement.span);
        while matches!(after.kind, Semicolon) {
            statement.span.end += after.len();
            after = self.lexer.semantic_token_after(&after.span);
        }

        return statement;
    }
    pub fn expression_after(&self, start: &Span, precedence: ExpressionPrecedence) -> Expression {
        let current = self.lexer.semantic_token_after(start);
        let mut left = match current.kind {
            True => Expression { span: current.span, kind: LiteralBoolean { value: true } },
            False => Expression { span: current.span, kind: LiteralBoolean { value: false } },

            Int { value } => Expression { span: current.span, kind: LiteralInteger { value } },

            Ident { name } => Expression { span: current.span, kind: Identifier { name } },

            Bang | Minus => {
                let expr = self.expression_after(&current.span, Prefix);
                Expression {
                    span: Span { start: current.span.start, end: expr.span.end },
                    kind: Unary { op: UnaryOp::try_from(&current).unwrap(), expr: expr.into() },
                }
            }

            Lparen => {
                let mut expr = self.expression_after(&current.span, Lowest);
                let right_par = self.lexer.semantic_token_after(&expr.span);
                expr.span.start = current.span.start;
                expr.span.end = right_par.span.end;
                match right_par.kind {
                    Rparen => expr,
                    _ => Expression {
                        span: expr.span,
                        kind: IllegalExpression { value: self.lexer.slice(&expr.span).into() },
                    },
                }
            }

            _ => Expression {
                // todo: read until Semicolon
                span: current.span,
                kind: IllegalExpression { value: self.lexer.slice(&current.span).into() },
            },
        };

        loop {
            let next_token = self.lexer.semantic_token_after(&left.span);
            let next_precedence = ExpressionPrecedence::from(&next_token);
            if precedence >= next_precedence {
                return left;
            }
            match BinaryOp::try_from(&next_token) {
                Err(()) => return left,
                Ok(kind) => {
                    let expr = self.expression_after(&next_token.span, next_precedence);
                    left = Expression {
                        span: Span { start: left.span.start, end: expr.span.end },
                        kind: Binary { op: kind, left: Box::from(left), right: Box::from(expr) },
                    }
                }
            }
        }
    }

    pub fn next_statement(&self) -> Statement {
        let statement = self.statement_after(&Span { start: 0, end: self.lexer.position.take() });
        self.lexer.move_to(&statement.span);
        return statement;
    }
}

impl<'a> From<&'a str> for Parser<'a> {
    fn from(value: &'a str) -> Self {
        return Parser { lexer: Lexer::from(value) };
    }
}

impl TryFrom<&Token<'_>> for UnaryOp {
    type Error = ();

    fn try_from(value: &Token) -> Result<Self, Self::Error> {
        match value.kind {
            Bang => Ok(UnaryOp::Not),
            Minus => Ok(UnaryOp::Minus),
            _ => Err(()),
        }
    }
}

impl From<&Token<'_>> for ExpressionPrecedence {
    fn from(value: &Token) -> Self {
        match value.kind {
            TokenKind::Plus | Minus => Sum,
            TokenKind::Asterisk | TokenKind::Slash => Product,
            TokenKind::Lt | TokenKind::Gt => LesserGreater,
            TokenKind::Equals | TokenKind::Differs => Equals,
            _ => Lowest,
        }
    }
}

impl TryFrom<&Token<'_>> for BinaryOp {
    type Error = ();

    fn try_from(value: &Token) -> Result<Self, Self::Error> {
        match value.kind {
            TokenKind::Plus => Ok(BinaryOp::Plus),
            Minus => Ok(BinaryOp::Minus),
            TokenKind::Asterisk => Ok(BinaryOp::Times),
            TokenKind::Slash => Ok(BinaryOp::Div),
            TokenKind::Lt => Ok(BinaryOp::Lesser),
            TokenKind::Gt => Ok(BinaryOp::Greater),
            TokenKind::Equals => Ok(BinaryOp::Equals),
            TokenKind::Differs => Ok(BinaryOp::Differs),
            _ => Err(()),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use BinaryOp::Plus;
    use StatementKind::{ExprStmt, LetStmt};

    #[test]
    fn parser_initialization() {
        let parser = Parser::from("1 + 2");
        assert_eq!(parser.lexer.position.take(), 0);
        parser.next_statement();
        assert_eq!(parser.lexer.position.take(), 5);
    }

    #[test]
    fn let_statements() {
        let input = "
        let x = 5;
        let y = 10;
        let foobar = 838383;
        ";

        let parser = Parser::from(input);

        assert_eq!(&input[9..19], "let x = 5;");
        assert_eq!(&input[17..18], "5");
        assert_eq!(
            parser.next_statement(),
            Statement {
                span: Span { start: 9, end: 19 },
                kind: LetStmt {
                    name: "x".into(),
                    expr: Expression {
                        span: Span { start: 17, end: 18 },
                        kind: LiteralInteger { value: 5 },
                    },
                },
            }
        );

        assert_eq!(&input[28..39], "let y = 10;");
        assert_eq!(&input[36..38], "10");
        assert_eq!(
            parser.next_statement(),
            Statement {
                span: Span { start: 28, end: 39 },
                kind: LetStmt {
                    name: "y".into(),
                    expr: Expression {
                        span: Span { start: 36, end: 38 },
                        kind: LiteralInteger { value: 10 },
                    },
                },
            }
        );

        assert_eq!(&input[48..68], "let foobar = 838383;");
        assert_eq!(&input[61..67], "838383");
        assert_eq!(
            parser.next_statement(),
            Statement {
                span: Span { start: 48, end: 68 },
                kind: LetStmt {
                    name: "foobar".into(),
                    expr: Expression {
                        span: Span { start: 61, end: 67 },
                        kind: LiteralInteger { value: 838383 },
                    },
                },
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

        let parser = Parser::from(input);

        assert_eq!(&input[9..18], "return 5;");
        assert_eq!(&input[16..17], "5");
        assert_eq!(
            parser.next_statement(),
            Statement {
                span: Span { start: 9, end: 18 },
                kind: ReturnStmt {
                    expr: Expression {
                        span: Span { start: 16, end: 17 },
                        kind: LiteralInteger { value: 5 },
                    }
                },
            }
        );

        assert_eq!(&input[27..37], "return 10;");
        assert_eq!(&input[34..36], "10");
        assert_eq!(
            parser.next_statement(),
            Statement {
                span: Span { start: 27, end: 37 },
                kind: ReturnStmt {
                    expr: Expression {
                        span: Span { start: 34, end: 36 },
                        kind: LiteralInteger { value: 10 },
                    }
                },
            }
        );

        assert_eq!(&input[46..60], "return 993322;");
        assert_eq!(&input[53..59], "993322");
        assert_eq!(
            parser.next_statement(),
            Statement {
                span: Span { start: 46, end: 60 },
                kind: ReturnStmt {
                    expr: Expression {
                        span: Span { start: 53, end: 59 },
                        kind: LiteralInteger { value: 993322 },
                    }
                },
            }
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

        let parser = Parser::from(input);

        assert_eq!(&input[9..14], "name;");
        assert_eq!(&input[9..13], "name");
        assert_eq!(
            parser.next_statement(),
            Statement {
                span: Span { start: 9, end: 14 },
                kind: ExprStmt {
                    expr: Expression {
                        span: Span { start: 9, end: 13 },
                        kind: Identifier { name: "name".into() },
                    }
                },
            }
        );

        assert_eq!(&input[23..27], "name");
        assert_eq!(&input[23..27], "name");
        assert_eq!(
            parser.next_statement(),
            Statement {
                span: Span { start: 23, end: 27 },
                kind: ExprStmt {
                    expr: Expression {
                        span: Span { start: 23, end: 27 },
                        kind: Identifier { name: "name".into() },
                    }
                },
            }
        );

        assert_eq!(&input[36..38], "5;");
        assert_eq!(&input[36..37], "5");
        assert_eq!(
            parser.next_statement(),
            Statement {
                span: Span { start: 36, end: 38 },
                kind: ExprStmt {
                    expr: Expression {
                        span: Span { start: 36, end: 37 },
                        kind: LiteralInteger { value: 5 },
                    }
                },
            }
        );

        assert_eq!(&input[47..48], "5");
        assert_eq!(&input[47..48], "5");
        assert_eq!(
            parser.next_statement(),
            Statement {
                span: Span { start: 47, end: 48 },
                kind: ExprStmt {
                    expr: Expression {
                        span: Span { start: 47, end: 48 },
                        kind: LiteralInteger { value: 5 },
                    }
                },
            }
        );

        assert_eq!(&input[57..60], "!5;");
        assert_eq!(&input[57..59], "!5");
        assert_eq!(&input[58..59], "5");
        assert_eq!(
            parser.next_statement(),
            Statement {
                span: Span { start: 57, end: 60 },
                kind: ExprStmt {
                    expr: Expression {
                        span: Span { start: 57, end: 59 },
                        kind: Unary {
                            op: UnaryOp::Not,
                            expr: Box::from(Expression {
                                span: Span { start: 58, end: 59 },
                                kind: LiteralInteger { value: 5 },
                            }),
                        },
                    }
                },
            }
        );

        assert_eq!(&input[69..73], "-15;");
        assert_eq!(&input[69..72], "-15");
        assert_eq!(&input[70..72], "15");
        assert_eq!(
            parser.next_statement(),
            Statement {
                span: Span { start: 69, end: 73 },
                kind: ExprStmt {
                    expr: Expression {
                        span: Span { start: 69, end: 72 },
                        kind: Unary {
                            op: UnaryOp::Minus,
                            expr: Box::from(Expression {
                                span: Span { start: 70, end: 72 },
                                kind: LiteralInteger { value: 15 },
                            }),
                        },
                    }
                },
            }
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

        let parser = Parser::from(input);

        assert_eq!(&input[9..15], "5 + 5;");
        assert_eq!(&input[9..14], "5 + 5");
        assert_eq!(&input[9..10], "5");
        assert_eq!(&input[13..14], "5");
        assert_eq!(
            parser.next_statement(),
            Statement {
                span: Span { start: 9, end: 15 },
                kind: ExprStmt {
                    expr: Expression {
                        span: Span { start: 9, end: 14 },
                        kind: Binary {
                            op: Plus,
                            left: Box::from(Expression {
                                span: Span { start: 9, end: 10 },
                                kind: LiteralInteger { value: 5 },
                            }),
                            right: Box::from(Expression {
                                span: Span { start: 13, end: 14 },
                                kind: LiteralInteger { value: 5 },
                            }),
                        },
                    }
                },
            }
        );

        assert_eq!(&input[24..30], "5 - 5;");
        assert_eq!(&input[24..29], "5 - 5");
        assert_eq!(&input[24..25], "5");
        assert_eq!(&input[28..29], "5");
        assert_eq!(
            parser.next_statement(),
            Statement {
                span: Span { start: 24, end: 30 },
                kind: ExprStmt {
                    expr: Expression {
                        span: Span { start: 24, end: 29 },
                        kind: Binary {
                            op: BinaryOp::Minus,
                            left: Box::from(Expression {
                                span: Span { start: 24, end: 25 },
                                kind: LiteralInteger { value: 5 },
                            }),
                            right: Box::from(Expression {
                                span: Span { start: 28, end: 29 },
                                kind: LiteralInteger { value: 5 },
                            }),
                        },
                    }
                },
            }
        );

        assert_eq!(&input[39..45], "5 * 5;");
        assert_eq!(&input[39..44], "5 * 5");
        assert_eq!(&input[39..40], "5");
        assert_eq!(&input[43..44], "5");
        assert_eq!(
            parser.next_statement(),
            Statement {
                span: Span { start: 39, end: 45 },
                kind: ExprStmt {
                    expr: Expression {
                        span: Span { start: 39, end: 44 },
                        kind: Binary {
                            op: BinaryOp::Times,
                            left: Box::from(Expression {
                                span: Span { start: 39, end: 40 },
                                kind: LiteralInteger { value: 5 },
                            }),
                            right: Box::from(Expression {
                                span: Span { start: 43, end: 44 },
                                kind: LiteralInteger { value: 5 },
                            }),
                        },
                    }
                },
            }
        );

        assert_eq!(&input[54..60], "5 / 5;");
        assert_eq!(&input[54..59], "5 / 5");
        assert_eq!(&input[54..55], "5");
        assert_eq!(&input[58..59], "5");
        assert_eq!(
            parser.next_statement(),
            Statement {
                span: Span { start: 54, end: 60 },
                kind: ExprStmt {
                    expr: Expression {
                        span: Span { start: 54, end: 59 },
                        kind: Binary {
                            op: BinaryOp::Div,
                            left: Box::from(Expression {
                                span: Span { start: 54, end: 55 },
                                kind: LiteralInteger { value: 5 },
                            }),
                            right: Box::from(Expression {
                                span: Span { start: 58, end: 59 },
                                kind: LiteralInteger { value: 5 },
                            }),
                        },
                    }
                },
            }
        );

        assert_eq!(&input[69..75], "5 > 5;");
        assert_eq!(&input[69..74], "5 > 5");
        assert_eq!(&input[69..70], "5");
        assert_eq!(&input[73..74], "5");
        assert_eq!(
            parser.next_statement(),
            Statement {
                span: Span { start: 69, end: 75 },
                kind: ExprStmt {
                    expr: Expression {
                        span: Span { start: 69, end: 74 },
                        kind: Binary {
                            op: BinaryOp::Greater,
                            left: Box::from(Expression {
                                span: Span { start: 69, end: 70 },
                                kind: LiteralInteger { value: 5 },
                            }),
                            right: Box::from(Expression {
                                span: Span { start: 73, end: 74 },
                                kind: LiteralInteger { value: 5 },
                            }),
                        },
                    }
                },
            }
        );

        assert_eq!(&input[84..90], "5 < 5;");
        assert_eq!(&input[84..89], "5 < 5");
        assert_eq!(&input[84..85], "5");
        assert_eq!(&input[88..89], "5");
        assert_eq!(
            parser.next_statement(),
            Statement {
                span: Span { start: 84, end: 90 },
                kind: ExprStmt {
                    expr: Expression {
                        span: Span { start: 84, end: 89 },
                        kind: Binary {
                            op: BinaryOp::Lesser,
                            left: Box::from(Expression {
                                span: Span { start: 84, end: 85 },
                                kind: LiteralInteger { value: 5 },
                            }),
                            right: Box::from(Expression {
                                span: Span { start: 88, end: 89 },
                                kind: LiteralInteger { value: 5 },
                            }),
                        },
                    }
                },
            }
        );

        assert_eq!(&input[99..106], "5 == 5;");
        assert_eq!(&input[99..105], "5 == 5");
        assert_eq!(&input[99..100], "5");
        assert_eq!(&input[104..105], "5");
        assert_eq!(
            parser.next_statement(),
            Statement {
                span: Span { start: 99, end: 106 },
                kind: ExprStmt {
                    expr: Expression {
                        span: Span { start: 99, end: 105 },
                        kind: Binary {
                            op: BinaryOp::Equals,
                            left: Box::from(Expression {
                                span: Span { start: 99, end: 100 },
                                kind: LiteralInteger { value: 5 },
                            }),
                            right: Box::from(Expression {
                                span: Span { start: 104, end: 105 },
                                kind: LiteralInteger { value: 5 },
                            }),
                        },
                    }
                },
            }
        );

        assert_eq!(&input[115..122], "5 != 5;");
        assert_eq!(&input[115..121], "5 != 5");
        assert_eq!(&input[115..116], "5");
        assert_eq!(&input[120..121], "5");
        assert_eq!(
            parser.next_statement(),
            Statement {
                span: Span { start: 115, end: 122 },
                kind: ExprStmt {
                    expr: Expression {
                        span: Span { start: 115, end: 121 },
                        kind: Binary {
                            op: BinaryOp::Differs,
                            left: Box::from(Expression {
                                span: Span { start: 115, end: 116 },
                                kind: LiteralInteger { value: 5 },
                            }),
                            right: Box::from(Expression {
                                span: Span { start: 120, end: 121 },
                                kind: LiteralInteger { value: 5 },
                            }),
                        },
                    }
                },
            }
        );
    }

    #[test]
    fn infix_precedence() {
        let input = "
        -a * b;
        !-a;
        a + b + c;
        ";

        let parser = Parser::from(input);

        assert_eq!(&input[9..16], "-a * b;");
        assert_eq!(&input[9..15], "-a * b");
        assert_eq!(&input[9..11], "-a");
        assert_eq!(&input[10..11], "a");
        assert_eq!(&input[14..15], "b");
        assert_eq!(
            parser.next_statement(),
            Statement {
                span: Span { start: 9, end: 16 },
                kind: ExprStmt {
                    expr: Expression {
                        span: Span { start: 9, end: 15 },
                        kind: Binary {
                            op: BinaryOp::Times,
                            left: Box::from(Expression {
                                span: Span { start: 9, end: 11 },
                                kind: Unary {
                                    op: UnaryOp::Minus,
                                    expr: Box::from(Expression {
                                        span: Span { start: 10, end: 11 },
                                        kind: Identifier { name: "a".into() },
                                    }),
                                },
                            }),
                            right: Box::from(Expression {
                                span: Span { start: 14, end: 15 },
                                kind: Identifier { name: "b".into() },
                            }),
                        },
                    }
                },
            }
        );

        assert_eq!(&input[25..29], "!-a;");
        assert_eq!(&input[25..28], "!-a");
        assert_eq!(&input[26..28], "-a");
        assert_eq!(&input[27..28], "a");
        assert_eq!(
            parser.next_statement(),
            Statement {
                span: Span { start: 25, end: 29 },
                kind: ExprStmt {
                    expr: Expression {
                        span: Span { start: 25, end: 28 },
                        kind: Unary {
                            op: UnaryOp::Not,
                            expr: Box::from(Expression {
                                span: Span { start: 26, end: 28 },
                                kind: Unary {
                                    op: UnaryOp::Minus,
                                    expr: Box::from(Expression {
                                        span: Span { start: 27, end: 28 },
                                        kind: Identifier { name: "a".into() },
                                    }),
                                },
                            }),
                        },
                    }
                },
            }
        );

        assert_eq!(&input[38..48], "a + b + c;");
        assert_eq!(&input[38..47], "a + b + c");
        assert_eq!(&input[38..43], "a + b");
        assert_eq!(&input[38..39], "a");
        assert_eq!(&input[42..43], "b");
        assert_eq!(&input[46..47], "c");
        assert_eq!(
            parser.next_statement(),
            Statement {
                span: Span { start: 38, end: 48 },
                kind: ExprStmt {
                    expr: Expression {
                        span: Span { start: 38, end: 47 },
                        kind: Binary {
                            op: Plus,
                            left: Box::from(Expression {
                                span: Span { start: 38, end: 43 },
                                kind: Binary {
                                    op: Plus,
                                    left: Box::from(Expression {
                                        span: Span { start: 38, end: 39 },
                                        kind: Identifier { name: "a".into() },
                                    }),
                                    right: Box::from(Expression {
                                        span: Span { start: 42, end: 43 },
                                        kind: Identifier { name: "b".into() },
                                    }),
                                },
                            }),
                            right: Box::from(Expression {
                                span: Span { start: 46, end: 47 },
                                kind: Identifier { name: "c".into() },
                            }),
                        },
                    }
                },
            }
        );
    }

    #[test]
    fn boolean_literals() {
        let input = "
        true;
        false;
        let foobar = true;
        let barfoo = false;
        ";

        let parser = Parser::from(input);

        assert_eq!(&input[9..14], "true;");
        assert_eq!(&input[9..13], "true");
        assert_eq!(
            parser.next_statement(),
            Statement {
                span: Span { start: 9, end: 14 },
                kind: ExprStmt {
                    expr: Expression {
                        span: Span { start: 9, end: 13 },
                        kind: LiteralBoolean { value: true },
                    }
                },
            }
        );

        assert_eq!(&input[23..29], "false;");
        assert_eq!(&input[23..28], "false");
        assert_eq!(
            parser.next_statement(),
            Statement {
                span: Span { start: 23, end: 29 },
                kind: ExprStmt {
                    expr: Expression {
                        span: Span { start: 23, end: 28 },
                        kind: LiteralBoolean { value: false },
                    }
                },
            }
        );

        assert_eq!(&input[38..56], "let foobar = true;");
        assert_eq!(&input[38..55], "let foobar = true");
        assert_eq!(&input[51..55], "true");
        assert_eq!(
            parser.next_statement(),
            Statement {
                span: Span { start: 38, end: 56 },
                kind: LetStmt {
                    name: "foobar".into(),
                    expr: Expression {
                        span: Span { start: 51, end: 55 },
                        kind: LiteralBoolean { value: true },
                    },
                },
            }
        );

        assert_eq!(&input[65..84], "let barfoo = false;");
        assert_eq!(&input[65..83], "let barfoo = false");
        assert_eq!(&input[78..83], "false");
        assert_eq!(
            parser.next_statement(),
            Statement {
                span: Span { start: 65, end: 84 },
                kind: LetStmt {
                    name: "barfoo".into(),
                    expr: Expression {
                        span: Span { start: 78, end: 83 },
                        kind: LiteralBoolean { value: false },
                    },
                },
            }
        );
    }

    #[test]
    fn grouped_expressions() {
        let input = "
        1 + (2 + 3) + 4;
        (5 + 5) * 2;
        2 / (5 + 5);
        -(5 + 5);
        !(true == true);
        ";

        let parser = Parser::from(input);

        assert_eq!(&input[9..25], "1 + (2 + 3) + 4;");
        assert_eq!(&input[9..24], "1 + (2 + 3) + 4");
        assert_eq!(&input[9..20], "1 + (2 + 3)");
        assert_eq!(&input[9..10], "1");
        assert_eq!(&input[13..20], "(2 + 3)");
        assert_eq!(&input[14..15], "2");
        assert_eq!(&input[18..19], "3");
        assert_eq!(&input[23..24], "4");
        assert_eq!(
            parser.next_statement(),
            Statement {
                span: Span { start: 9, end: 25 },
                kind: ExprStmt {
                    expr: Expression {
                        span: Span { start: 9, end: 24 },
                        kind: Binary {
                            op: Plus,
                            left: Box::from(Expression {
                                span: Span { start: 9, end: 20 },
                                kind: Binary {
                                    op: Plus,
                                    left: Box::from(Expression {
                                        span: Span { start: 9, end: 10 },
                                        kind: LiteralInteger { value: 1 },
                                    }),
                                    right: Box::from(Expression {
                                        span: Span { start: 13, end: 20 },
                                        kind: Binary {
                                            op: Plus,
                                            left: Box::from(Expression {
                                                span: Span { start: 14, end: 15 },
                                                kind: LiteralInteger { value: 2 },
                                            }),
                                            right: Box::from(Expression {
                                                span: Span { start: 18, end: 19 },
                                                kind: LiteralInteger { value: 3 },
                                            }),
                                        },
                                    }),
                                },
                            }),
                            right: Box::from(Expression {
                                span: Span { start: 23, end: 24 },
                                kind: LiteralInteger { value: 4 },
                            }),
                        },
                    },
                },
            }
        );
    }
}
