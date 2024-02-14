use crate::re_lexer::{real_token_after, Range, RawToken, TokenKind};

#[derive(Debug, PartialEq, Eq, Clone, Copy, Ord, PartialOrd)]
pub struct StatementId(usize);

#[derive(Debug, PartialEq, Eq, Clone, Copy, Ord, PartialOrd)]
pub struct ExpressionId(usize);

pub struct RawStatement {
    pub range: Range,
    pub kind: RawStatementKind,
}

pub struct RawExpression {
    pub range: Range,
    pub kind: RawExpressionKind,
}

pub enum RawStatementKind {
    LetStmt { name: ExpressionId, expr: ExpressionId },
    ReturnStmt { expr: ExpressionId },
    ExprStmt { expr: ExpressionId },
    IllegalStatement,
    EndStatement,
}

pub enum RawExpressionKind {
    LiteralInteger,
    LiteralString,
    LiteralBoolean,
    LiteralFunction {
        parameters: Vec<ExpressionId>,
        body: Vec<StatementId>,
    },
    LiteralArray {
        values: Vec<ExpressionId>,
    },
    LiteralHash {
        values: Vec<(ExpressionId, ExpressionId)>,
    },
    Identifier,
    Unary {
        op: UnaryOp,
        expr: ExpressionId,
    },
    Binary {
        op: BinaryOp,
        left: ExpressionId,
        right: ExpressionId,
    },
    Conditional {
        condition: ExpressionId,
        positive: Vec<StatementId>,
        negative: Vec<StatementId>,
    },
    Call {
        function: ExpressionId,
        arguments: Vec<ExpressionId>,
    },
    IndexExpression {
        left: ExpressionId,
        idx: ExpressionId,
    },
    IllegalExpression,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Ord, PartialOrd)]
pub enum UnaryOp {
    OpNot,
    OpNeg,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Ord, PartialOrd)]
pub enum BinaryOp {
    OpPlus,
    OpMinus,
    OpTimes,
    OpDiv,
    OpGreater,
    OpLesser,
    OpEquals,
    OpDiffers,
}

#[derive(Clone, Copy, PartialOrd, PartialEq)]
pub enum Precedence {
    Lowest,
    Equals,
    LesserGreater,
    Sum,
    Product,
    Prefix,
    Apply,
    Index,
}

struct ExprTable {
    statements: Vec<RawStatement>,
    expressions: Vec<RawExpression>,
}

impl ExprTable {
    fn add_statement(&mut self, statement: RawStatement) -> StatementId {
        let id = self.statements.len();
        self.statements.push(statement);
        return StatementId(id);
    }

    fn add_expression(&mut self, expression: RawExpression) -> ExpressionId {
        let id = self.expressions.len();
        self.expressions.push(expression);
        return ExpressionId(id);
    }

    fn get_statement(&self, id: &StatementId) -> &RawStatement {
        self.statements.get(id.0).unwrap()
    }

    fn get_expression(&self, id: &ExpressionId) -> &RawExpression {
        self.expressions.get(id.0).unwrap()
    }
}

fn expression_after(
    input: &str,
    range: &Range,
    table: &mut ExprTable,
    precedence: Precedence,
) -> RawExpression {
    match real_token_after(input, range) {
        RawToken {
            kind: TokenKind::True | TokenKind::False,
            range,
        } => RawExpression {
            range,
            kind: RawExpressionKind::LiteralBoolean,
        },

        RawToken { kind: TokenKind::Int, range } => RawExpression {
            range,
            kind: RawExpressionKind::LiteralInteger,
        },

        RawToken { kind: TokenKind::Ident, range } => {
            RawExpression { range, kind: RawExpressionKind::Identifier }
        }

        RawToken { kind: TokenKind::Str, range } => {
            RawExpression { range, kind: RawExpressionKind::LiteralString }
        }

        RawToken { kind, ref range } if matches!(kind, TokenKind::Bang | TokenKind::Minus) => {
            unary_expression(input, range, kind, table)
        }

        RawToken { kind: TokenKind::Lparen, .. } => parenthesized_expression(input, range, table),
        _ => todo!(),
    }
}

fn parenthesized_expression(input: &str, range: &Range, kind: &mut ExprTable) -> RawExpression {
    let l_paren = {
        match real_token_after(input, range) {
            t @ RawToken { kind: TokenKind::Lparen, .. } => t,
            t => {
                return RawExpression {
                    range: t.range,
                    kind: RawExpressionKind::IllegalExpression,
                }
            }
        }
    };

    let expr = expression_after(input, &l_paren.range, kind, Precedence::Lowest);

    let r_paren = {
        match real_token_after(input, &expr.range) {
            t @ RawToken { kind: TokenKind::Rparen, .. } => t,
            t => {
                return RawExpression {
                    range: t.range,
                    kind: RawExpressionKind::IllegalExpression,
                }
            }
        }
    };

    RawExpression {
        range: Range::merge(range, &r_paren.range),
        ..expr
    }
}

fn unary_expression(
    input: &str,
    range: &Range,
    kind: TokenKind,
    table: &mut ExprTable,
) -> RawExpression {
    let op = match kind {
        TokenKind::Bang => UnaryOp::OpNot,
        TokenKind::Minus => UnaryOp::OpNeg,
        _ => unreachable!(),
    };

    let expr = expression_after(input, range, table, Precedence::Prefix);
    RawExpression {
        range: Range::merge(range, &expr.range),
        kind: RawExpressionKind::Unary { op, expr: table.add_expression(expr) },
    }
}

fn statement_after(input: &str, range: &Range, table: &mut ExprTable) -> RawStatement {
    let statement = match real_token_after(input, range) {
        RawToken { kind: TokenKind::Return, ref range } => return_statement(input, range, table),
        RawToken { kind: TokenKind::Let, ref range } => let_statement(input, range, table),
        _ => expression_statement(input, range, table),
    };

    match real_token_after(input, &statement.range) {
        RawToken { kind: TokenKind::Semicolon, ref range } => RawStatement {
            range: Range::merge(&statement.range, range),
            ..statement
        },
        _ => statement,
    }
}

fn let_statement(input: &str, start: &Range, table: &mut ExprTable) -> RawStatement {
    let ident = {
        let expression = expression_after(input, start, table, Precedence::Lowest);
        if !matches!(expression.kind, RawExpressionKind::Identifier) {
            return RawStatement {
                range: expression.range,
                kind: RawStatementKind::IllegalStatement,
            };
        }
        expression
    };

    let eq = {
        let token = real_token_after(input, &ident.range);
        if !matches!(token.kind, TokenKind::Assign) {
            return RawStatement {
                range: Range::merge(start, &ident.range),
                kind: RawStatementKind::IllegalStatement,
            };
        }
        token
    };

    let expr = expression_after(input, &eq.range, table, Precedence::Lowest);
    return RawStatement {
        range: Range::merge(start, &expr.range),
        kind: RawStatementKind::LetStmt {
            name: table.add_expression(ident),
            expr: table.add_expression(expr),
        },
    };
}

fn return_statement(input: &str, start: &Range, table: &mut ExprTable) -> RawStatement {
    let expression = expression_after(input, start, table, Precedence::Lowest);
    RawStatement {
        range: Range::merge(start, &expression.range),
        kind: RawStatementKind::ReturnStmt { expr: table.add_expression(expression) },
    }
}

fn expression_statement(input: &str, start: &Range, table: &mut ExprTable) -> RawStatement {
    let expr = expression_after(input, start, table, Precedence::Lowest);
    return RawStatement {
        range: expr.range,
        kind: RawStatementKind::ExprStmt { expr: table.add_expression(expr) },
    };
}

#[cfg(test)]
mod tests {
    use crate::re_lexer::Range;
    use crate::re_parser::{
        statement_after, BinaryOp, ExprTable, ExpressionId, RawExpression, RawExpressionKind,
        RawStatement, RawStatementKind, StatementId, UnaryOp,
    };

    #[derive(Debug, PartialEq, Eq, Clone, Ord, PartialOrd)]
    struct Statement {
        content: String,
        kind: StatementKind,
    }

    #[derive(Debug, PartialEq, Eq, Clone, Ord, PartialOrd)]
    struct Expression {
        content: String,
        kind: ExpressionKind,
    }

    #[derive(Debug, PartialEq, Eq, Clone, Ord, PartialOrd)]
    enum StatementKind {
        LetStmt { name: Expression, expr: Expression },
        ReturnStmt { expr: Expression },
        ExprStmt { expr: Expression },
        IllegalStatement,
        EndStatement,
    }

    #[derive(Debug, PartialEq, Eq, Clone, Ord, PartialOrd)]
    enum ExpressionKind {
        LiteralInteger,
        LiteralString,
        LiteralBoolean,
        LiteralFunction {
            parameters: Vec<Expression>,
            body: Vec<Statement>,
        },
        LiteralArray {
            values: Vec<Expression>,
        },
        LiteralHash {
            values: Vec<(Expression, Expression)>,
        },
        Identifier,
        Unary {
            op: UnaryOp,
            expr: Box<Expression>,
        },
        Binary {
            op: BinaryOp,
            left: Box<Expression>,
            right: Box<Expression>,
        },
        Conditional {
            condition: Box<Expression>,
            positive: Vec<Statement>,
            negative: Vec<Statement>,
        },
        Call {
            function: Box<Expression>,
            arguments: Vec<Expression>,
        },
        IndexExpression {
            left: Box<Expression>,
            idx: Box<Expression>,
        },
        IllegalExpression,
    }

    fn parse(input: &str) -> Statement {
        let mut table = ExprTable {
            statements: Vec::new(),
            expressions: Vec::new(),
        };
        let range = Range::new(0, 0);
        let statement = statement_after(input, &range, &mut table);

        return Statement {
            content: input[statement.range].into(),
            kind: match statement.kind {
                RawStatementKind::EndStatement => StatementKind::EndStatement {},
                RawStatementKind::IllegalStatement => StatementKind::IllegalStatement {},
                RawStatementKind::LetStmt { name, expr } => StatementKind::LetStmt {
                    name: lookup_expression(input, &name, &table),
                    expr: lookup_expression(input, &expr, &table),
                },
                RawStatementKind::ReturnStmt { expr } => {
                    StatementKind::ReturnStmt { expr: lookup_expression(input, &expr, &table) }
                }
                RawStatementKind::ExprStmt { expr } => {
                    StatementKind::ExprStmt { expr: lookup_expression(input, &expr, &table) }
                }
            },
        };
    }

    fn lookup_expression(input: &str, id: &ExpressionId, table: &ExprTable) -> Expression {
        let RawExpression { range, kind } = table.get_expression(id);
        return Expression {
            content: input[range.start..range.end].into(),
            kind: match kind {
                RawExpressionKind::LiteralInteger => ExpressionKind::LiteralInteger,
                RawExpressionKind::LiteralString => ExpressionKind::LiteralString,
                RawExpressionKind::LiteralBoolean => ExpressionKind::LiteralBoolean,
                RawExpressionKind::Identifier => ExpressionKind::Identifier,
                RawExpressionKind::IllegalExpression => ExpressionKind::IllegalExpression,
                RawExpressionKind::LiteralFunction { parameters, body } => {
                    ExpressionKind::LiteralFunction {
                        parameters: parameters
                            .iter()
                            .map(|p| lookup_expression(input, p, table))
                            .collect(),
                        body: body
                            .iter()
                            .map(|p| lookup_statement(input, p, table))
                            .collect(),
                    }
                }
                RawExpressionKind::LiteralArray { values } => ExpressionKind::LiteralArray {
                    values: values
                        .iter()
                        .map(|p| lookup_expression(input, p, table))
                        .collect(),
                },
                RawExpressionKind::LiteralHash { values } => ExpressionKind::LiteralHash {
                    values: values
                        .iter()
                        .map(|(k, v)| {
                            (
                                lookup_expression(input, k, table),
                                lookup_expression(input, v, table),
                            )
                        })
                        .collect(),
                },
                RawExpressionKind::Unary { op, expr } => ExpressionKind::Unary {
                    op: *op,
                    expr: lookup_expression(input, expr, table).into(),
                },
                RawExpressionKind::Binary { op, left, right } => ExpressionKind::Binary {
                    op: *op,
                    left: lookup_expression(input, left, table).into(),
                    right: lookup_expression(input, right, table).into(),
                },
                RawExpressionKind::Conditional { condition, positive, negative } => {
                    ExpressionKind::Conditional {
                        condition: lookup_expression(input, condition, table).into(),
                        positive: positive
                            .iter()
                            .map(|p| lookup_statement(input, p, table))
                            .collect(),
                        negative: negative
                            .iter()
                            .map(|p| lookup_statement(input, p, table))
                            .collect(),
                    }
                }
                RawExpressionKind::Call { function, arguments } => ExpressionKind::Call {
                    function: lookup_expression(input, function, table).into(),
                    arguments: arguments
                        .iter()
                        .map(|a| lookup_expression(input, a, table))
                        .collect(),
                },
                RawExpressionKind::IndexExpression { left, idx } => {
                    ExpressionKind::IndexExpression {
                        left: lookup_expression(input, left, table).into(),
                        idx: lookup_expression(input, idx, table).into(),
                    }
                }
            },
        };
    }

    fn lookup_statement(input: &str, id: &StatementId, table: &ExprTable) -> Statement {
        let RawStatement { range, kind } = table.get_statement(id);
        Statement {
            content: input[range.start..range.end].into(),
            kind: match kind {
                RawStatementKind::LetStmt { name, expr } => StatementKind::LetStmt {
                    name: lookup_expression(input, name, table),
                    expr: lookup_expression(input, expr, table),
                },
                RawStatementKind::ReturnStmt { expr } => {
                    StatementKind::ReturnStmt { expr: lookup_expression(input, expr, table) }
                }
                RawStatementKind::ExprStmt { expr } => {
                    StatementKind::ExprStmt { expr: lookup_expression(input, expr, table) }
                }
                RawStatementKind::IllegalStatement => StatementKind::IllegalStatement,
                RawStatementKind::EndStatement => StatementKind::EndStatement,
            },
        }
    }

    #[test]
    fn return_statements() {
        assert_eq!(
            parse("return true"),
            Statement {
                content: "return true".into(),
                kind: StatementKind::ReturnStmt {
                    expr: Expression {
                        content: "true".into(),
                        kind: ExpressionKind::LiteralBoolean,
                    },
                },
            }
        );

        assert_eq!(
            parse("return false;"),
            Statement {
                content: "return false;".into(),
                kind: StatementKind::ReturnStmt {
                    expr: Expression {
                        content: "false".into(),
                        kind: ExpressionKind::LiteralBoolean,
                    },
                },
            }
        )
    }

    #[test]
    fn let_statements() {
        assert_eq!(
            parse("let a = true"),
            Statement {
                content: "let a = true".into(),
                kind: StatementKind::LetStmt {
                    name: Expression {
                        content: "a".to_string(),
                        kind: ExpressionKind::Identifier,
                    },
                    expr: Expression {
                        content: "true".to_string(),
                        kind: ExpressionKind::LiteralBoolean,
                    },
                },
            }
        );

        assert_eq!(
            parse("let b = false;"),
            Statement {
                content: "let b = false;".into(),
                kind: StatementKind::LetStmt {
                    name: Expression {
                        content: "b".to_string(),
                        kind: ExpressionKind::Identifier,
                    },
                    expr: Expression {
                        content: "false".to_string(),
                        kind: ExpressionKind::LiteralBoolean,
                    },
                },
            }
        )
    }

    #[test]
    fn expr_statements() {
        assert_eq!(
            parse("2"),
            Statement {
                content: "2".into(),
                kind: StatementKind::ExprStmt {
                    expr: Expression {
                        content: "2".to_string(),
                        kind: ExpressionKind::LiteralInteger,
                    },
                },
            }
        );
    }

    #[test]
    fn unary_expressions() {
        assert_eq!(
            parse("-2"),
            Statement {
                content: "-2".into(),
                kind: StatementKind::ExprStmt {
                    expr: Expression {
                        content: "-2".to_string(),
                        kind: ExpressionKind::Unary {
                            op: UnaryOp::OpNeg,
                            expr: Box::new(Expression {
                                content: "2".to_string(),
                                kind: ExpressionKind::LiteralInteger,
                            }),
                        },
                    },
                },
            }
        );

        assert_eq!(
            parse("!true;"),
            Statement {
                content: "!true;".into(),
                kind: StatementKind::ExprStmt {
                    expr: Expression {
                        content: "!true".to_string(),
                        kind: ExpressionKind::Unary {
                            op: UnaryOp::OpNot,
                            expr: Box::new(Expression {
                                content: "true".to_string(),
                                kind: ExpressionKind::LiteralBoolean,
                            }),
                        },
                    },
                },
            }
        );
    }

    #[test]
    fn parenthesized_expressions() {
        assert_eq!(
            parse("(-2)"),
            Statement {
                content: "(-2)".into(),
                kind: StatementKind::ExprStmt {
                    expr: Expression {
                        content: "(-2)".to_string(),
                        kind: ExpressionKind::Unary {
                            op: UnaryOp::OpNeg,
                            expr: Box::new(Expression {
                                content: "2".to_string(),
                                kind: ExpressionKind::LiteralInteger,
                            }),
                        },
                    },
                },
            }
        );
    }

}
