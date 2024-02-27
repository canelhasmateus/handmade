use crate::re_lexer::{token_after, Range, RawToken, TokenKind};

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

impl From<&RawToken> for Precedence {
    fn from(value: &RawToken) -> Self {
        match value.kind {
            TokenKind::Plus | TokenKind::Minus => Precedence::Sum,
            TokenKind::Asterisk | TokenKind::Slash => Precedence::Product,
            TokenKind::Lt | TokenKind::Gt => Precedence::LesserGreater,
            TokenKind::Equals | TokenKind::Differs => Precedence::Equals,
            TokenKind::Lparen => Precedence::Apply,
            TokenKind::LBracket => Precedence::Index,
            _ => Precedence::Lowest,
        }
    }
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
    let mut left_expr = match token_after(input, range) {
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

        RawToken { kind: TokenKind::If, ref range } => conditional_expression(input, range, table),

        RawToken { kind: TokenKind::Function, ref range } => {
            function_expression(input, range, table)
        }

        RawToken { kind, ref range } if matches!(kind, TokenKind::Bang | TokenKind::Minus) => {
            unary_expression(input, range, kind, table)
        }

        RawToken { kind: TokenKind::Lparen, .. } => parenthesized_expression(input, range, table),
        RawToken { kind: TokenKind::Lbrace, .. } => hash_expression(input, range, table),
        RawToken { kind: TokenKind::LBracket, .. } => array_expression(input, range, table),

        RawToken { range, .. } => RawExpression {
            range,
            kind: RawExpressionKind::LiteralInteger,
        },
    };

    loop {
        let next_token = token_after(input, &left_expr.range);
        let next_precedence = Precedence::from(&next_token);
        if precedence >= next_precedence {
            break;
        }

        left_expr = match next_token.kind {
            TokenKind::Lparen => call_expression(input, left_expr, table),
            TokenKind::LBracket => index_expression(input, left_expr, table),
            // TokenKind::LBracket => {
            // let idx = self.expression_after(&next_token.span, ExpressionPrecedence::Lowest);
            // let after = self.lexer.semantic_token_after(&idx.span);
            // if after.kind != Rbracket {
            //     return Expression {
            //         span: Span {
            //             start: next_token.span.start,
            //             end: after.span.end,
            //         },
            //         kind: IllegalExpression { value: "illegal".into() },
            //     };
            // }
            // return Expression {
            //     span: Span {
            //         start: next_token.span.start,
            //         end: after.span.end,
            //     },
            //     kind: IndexExpression { left: Box::from(left), idx: Box::from(idx) },
            // };
            // }
            _ => todo!(),
        }
    }

    return left_expr;
}

fn index_expression(input: &str, left: RawExpression, table: &mut ExprTable) -> RawExpression {
    let left_bracket = match expect_token(input, &left.range, &left.range, TokenKind::LBracket) {
        Ok(t) => t,
        Err(e) => return e,
    };

    let expr = expression_after(input, &left_bracket.range, table, Precedence::Index);

    let right_bracket = match expect_token(input, &left.range, &expr.range, TokenKind::RBracket) {
        Ok(t) => t,
        Err(e) => return e,
    };

    RawExpression {
        range: Range::merge(&left.range, &right_bracket.range),
        kind: RawExpressionKind::IndexExpression {
            left: table.add_expression(left),
            idx: table.add_expression(expr),
        },
    }
}

fn call_expression(input: &str, left: RawExpression, table: &mut ExprTable) -> RawExpression {
    let left_paren = match expect_token(input, &left.range, &left.range, TokenKind::Lparen) {
        Ok(t) => t,
        Err(e) => return e,
    };
    let ExpressionList { ref range, expressions } =
        expression_list(input, &left_paren.range, table, TokenKind::Rparen);

    let right_paren = match expect_token(input, &left.range, range, TokenKind::Rparen) {
        Ok(t) => t,
        Err(e) => return e,
    };

    RawExpression {
        range: Range::merge(&left.range, &right_paren.range),
        kind: RawExpressionKind::Call {
            function: table.add_expression(left),
            arguments: expressions,
        },
    }
}

fn array_expression(input: &str, start: &Range, table: &mut ExprTable) -> RawExpression {
    let left_brace = match expect_token(input, start, start, TokenKind::LBracket) {
        Ok(token) => token,
        Err(expr) => return expr,
    };

    let ExpressionList { ref range, expressions } =
        expression_list(input, &left_brace.range, table, TokenKind::RBracket);

    let right_brace = match expect_token(input, start, range, TokenKind::RBracket) {
        Ok(token) => token,
        Err(expr) => return expr,
    };

    RawExpression {
        range: Range::merge(start, &right_brace.range),
        kind: RawExpressionKind::LiteralArray { values: expressions },
    }
}

fn hash_expression(input: &str, start: &Range, table: &mut ExprTable) -> RawExpression {
    let left_brace = match expect_token(input, start, start, TokenKind::Lbrace) {
        Ok(token) => token,
        Err(expr) => return expr,
    };

    let mut values: Vec<(ExpressionId, ExpressionId)> = vec![];
    let mut current = left_brace.range;
    loop {
        if let RawToken { kind: TokenKind::Rbrace, range } = token_after(input, &current) {
            current = range;
            break;
        }

        let key = expression_after(input, &current, table, Precedence::Lowest);
        let colon = match expect_token(input, start, &key.range, TokenKind::Colon) {
            Ok(t) => t,
            Err(e) => return e,
        };
        let value = expression_after(input, &colon.range, table, Precedence::Lowest);
        if let RawToken { kind: TokenKind::Comma, range } = token_after(input, &value.range) {
            current = range
        } else {
            current = value.range;
        }

        values.push((table.add_expression(key), table.add_expression(value)));
    }

    RawExpression {
        range: Range::merge(start, &current),
        kind: RawExpressionKind::LiteralHash { values },
    }
}

fn function_expression(input: &str, start: &Range, table: &mut ExprTable) -> RawExpression {
    let left_paren = match expect_token(input, start, start, TokenKind::Lparen) {
        Ok(token) => token,
        Err(expr) => return expr,
    };

    let ExpressionList { expressions, range } =
        expression_list(input, &left_paren.range, table, TokenKind::Rparen);

    let right_paren = match expect_token(input, start, &range, TokenKind::Rparen) {
        Ok(token) => token,
        Err(expr) => return expr,
    };

    let StatementBlock { statements, ref range } =
        match statement_block(input, &right_paren.range, table) {
            Ok(block) => block,
            Err(e) => return e,
        };

    RawExpression {
        range: Range::merge(start, range),
        kind: RawExpressionKind::LiteralFunction { parameters: expressions, body: statements },
    }
}

fn conditional_expression(input: &str, start: &Range, table: &mut ExprTable) -> RawExpression {
    let left_paren = match expect_token(input, start, start, TokenKind::Lparen) {
        Ok(token) => token,
        Err(expr) => return expr,
    };

    let condition = expression_after(input, &left_paren.range, table, Precedence::Lowest);

    let right_paren = match expect_token(input, start, &condition.range, TokenKind::Rparen) {
        Ok(token) => token,
        Err(expr) => return expr,
    };

    let positive = match statement_block(input, &right_paren.range, table) {
        Ok(o) => o,
        Err(expr) => return expr,
    };

    let StatementBlock { statements, ref range } = {
        let RawToken { kind, ref range } = token_after(input, &positive.range);
        if kind != TokenKind::Else {
            StatementBlock { statements: vec![], range: positive.range }
        } else {
            match statement_block(input, range, table) {
                Ok(t) => t,
                Err(expr) => return expr,
            }
        }
    };

    RawExpression {
        range: Range::merge(start, range),
        kind: RawExpressionKind::Conditional {
            condition: table.add_expression(condition),
            positive: positive.statements,
            negative: statements,
        },
    }
}

fn parenthesized_expression(input: &str, start: &Range, kind: &mut ExprTable) -> RawExpression {
    let left_paren = match expect_token(input, start, start, TokenKind::Lparen) {
        Ok(token) => token,
        Err(expr) => return expr,
    };

    let expr = expression_after(input, &left_paren.range, kind, Precedence::Lowest);

    let right_paren = match expect_token(input, start, &expr.range, TokenKind::Rparen) {
        Ok(token) => token,
        Err(expr) => return expr,
    };

    RawExpression {
        range: Range::merge(start, &right_paren.range),
        ..expr
    }
}

fn unary_expression(
    input: &str,
    start: &Range,
    kind: TokenKind,
    table: &mut ExprTable,
) -> RawExpression {
    let op = match kind {
        TokenKind::Bang => UnaryOp::OpNot,
        TokenKind::Minus => UnaryOp::OpNeg,
        _ => unreachable!(),
    };

    let expr = expression_after(input, start, table, Precedence::Prefix);
    RawExpression {
        range: Range::merge(start, &expr.range),
        kind: RawExpressionKind::Unary { op, expr: table.add_expression(expr) },
    }
}

fn statement_after(input: &str, start: &Range, table: &mut ExprTable) -> RawStatement {
    let statement = match token_after(input, start) {
        RawToken { kind: TokenKind::Return, ref range } => return_statement(input, range, table),
        RawToken { kind: TokenKind::Let, ref range } => let_statement(input, range, table),
        _ => expression_statement(input, start, table),
    };

    match token_after(input, &statement.range) {
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
        let token = token_after(input, &ident.range);
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

struct StatementBlock {
    statements: Vec<StatementId>,
    range: Range,
}

struct ExpressionList {
    range: Range,
    expressions: Vec<ExpressionId>,
}

fn statement_block(
    input: &str,
    start: &Range,
    table: &mut ExprTable,
) -> Result<StatementBlock, RawExpression> {
    let mut res: Vec<StatementId> = Vec::new();

    let left_brace = match expect_token(input, start, start, TokenKind::Lbrace) {
        Ok(token) => token,
        Err(expr) => return Err(expr),
    };

    let mut current = left_brace.range;
    loop {
        match expect_token(input, start, &current, TokenKind::Rbrace) {
            Ok(_) => break,
            Err(_) => {
                let statement = statement_after(input, &current, table);
                current = statement.range;
                res.push(table.add_statement(statement));
            }
        }
    }

    let right_brace = match expect_token(input, start, &current, TokenKind::Rbrace) {
        Ok(token) => token,
        Err(expr) => return Err(expr),
    };

    return Ok(StatementBlock {
        range: Range::merge(start, &right_brace.range),
        statements: res,
    });
}

fn expression_list(
    input: &str,
    start: &Range,
    table: &mut ExprTable,
    end: TokenKind,
) -> ExpressionList {
    let mut res: Vec<ExpressionId> = vec![];
    let mut current = Range::merge(start, start);
    loop {
        match token_after(input, &current) {
            t if t.kind == end => break,
            t if t.kind == TokenKind::Comma => current = t.range,
            _ => {
                let expression = expression_after(input, &current, table, Precedence::Lowest);
                current = expression.range;
                res.push(table.add_expression(expression))
            }
        }
    }

    ExpressionList {
        range: Range::merge(start, &current),
        expressions: res,
    }
}

fn expect_token(
    input: &str,
    start: &Range,
    range: &Range,
    expected: TokenKind,
) -> Result<RawToken, RawExpression> {
    match token_after(input, range) {
        t if t.kind == expected => Ok(t),
        RawToken { kind, ref range } => Err(RawExpression {
            range: Range::merge(start, range),
            kind: RawExpressionKind::IllegalExpression,
        }),
    }
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
            parameters: Vec<String>,
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
                            .map(|e| e.content)
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

    #[test]
    fn conditional_expressions() {
        assert_eq!(
            parse(r#"if ( true ) { return true }"#),
            Statement {
                content: "if ( true ) { return true }".into(),
                kind: StatementKind::ExprStmt {
                    expr: Expression {
                        content: "if ( true ) { return true }".to_string(),
                        kind: ExpressionKind::Conditional {
                            condition: Box::new(Expression {
                                content: "true".to_string(),
                                kind: ExpressionKind::LiteralBoolean,
                            }),
                            positive: vec!(Statement {
                                content: "return true".to_string(),
                                kind: StatementKind::ReturnStmt {
                                    expr: Expression {
                                        content: "true".to_string(),
                                        kind: ExpressionKind::LiteralBoolean,
                                    }
                                },
                            }),
                            negative: vec![],
                        },
                    },
                },
            }
        );

        assert_eq!(
            parse(r#"if ( false ) { return true } else { return false }"#),
            Statement {
                content: "if ( false ) { return true } else { return false }".into(),
                kind: StatementKind::ExprStmt {
                    expr: Expression {
                        content: "if ( false ) { return true } else { return false }".to_string(),
                        kind: ExpressionKind::Conditional {
                            condition: Box::new(Expression {
                                content: "false".to_string(),
                                kind: ExpressionKind::LiteralBoolean,
                            }),
                            positive: vec!(Statement {
                                content: "return true".to_string(),
                                kind: StatementKind::ReturnStmt {
                                    expr: Expression {
                                        content: "true".to_string(),
                                        kind: ExpressionKind::LiteralBoolean,
                                    }
                                },
                            }),
                            negative: vec!(Statement {
                                content: "return false".to_string(),
                                kind: StatementKind::ReturnStmt {
                                    expr: Expression {
                                        content: "false".to_string(),
                                        kind: ExpressionKind::LiteralBoolean,
                                    }
                                },
                            }),
                        },
                    },
                },
            }
        );
    }

    #[test]
    fn function_expressions() {
        assert_eq!(
            parse(r#"fn(a, b){ return true }"#),
            Statement {
                content: "fn(a, b){ return true }".into(),
                kind: StatementKind::ExprStmt {
                    expr: Expression {
                        content: "fn(a, b){ return true }".to_string(),
                        kind: ExpressionKind::LiteralFunction {
                            parameters: vec!["a".into(), "b".into()],
                            body: vec![Statement {
                                content: "return true".to_string(),
                                kind: StatementKind::ReturnStmt {
                                    expr: Expression {
                                        content: "true".to_string(),
                                        kind: ExpressionKind::LiteralBoolean,
                                    }
                                },
                            }],
                        },
                    },
                },
            }
        );
    }

    #[test]
    fn hash_expressions() {
        assert_eq!(
            parse(r#"{"one" : -1, }"#),
            Statement {
                content: r#"{"one" : -1, }"#.into(),
                kind: StatementKind::ExprStmt {
                    expr: Expression {
                        content: r#"{"one" : -1, }"#.to_string(),
                        kind: ExpressionKind::LiteralHash {
                            values: vec![(
                                Expression {
                                    content: r#""one""#.to_string(),
                                    kind: ExpressionKind::LiteralString
                                },
                                Expression {
                                    content: "-1".to_string(),
                                    kind: ExpressionKind::Unary {
                                        op: UnaryOp::OpNeg,
                                        expr: Box::new(Expression {
                                            content: "1".to_string(),
                                            kind: ExpressionKind::LiteralInteger,
                                        }),
                                    },
                                }
                            )],
                        },
                    },
                },
            }
        );
    }

    #[test]
    fn array_expressions() {
        assert_eq!(
            parse(r#"[-1, "one", fn(a, b){ return true }]"#),
            Statement {
                content: r#"[-1, "one", fn(a, b){ return true }]"#.into(),
                kind: StatementKind::ExprStmt {
                    expr: Expression {
                        content: r#"[-1, "one", fn(a, b){ return true }]"#.to_string(),
                        kind: ExpressionKind::LiteralArray {
                            values: vec![
                                Expression {
                                    content: "-1".to_string(),
                                    kind: ExpressionKind::Unary {
                                        op: UnaryOp::OpNeg,
                                        expr: Box::new(Expression {
                                            content: "1".to_string(),
                                            kind: ExpressionKind::LiteralInteger,
                                        }),
                                    },
                                },
                                Expression {
                                    content: r#""one""#.to_string(),
                                    kind: ExpressionKind::LiteralString,
                                },
                                Expression {
                                    content: "fn(a, b){ return true }".to_string(),
                                    kind: ExpressionKind::LiteralFunction {
                                        parameters: vec!["a".into(), "b".into()],
                                        body: vec![Statement {
                                            content: "return true".to_string(),
                                            kind: StatementKind::ReturnStmt {
                                                expr: Expression {
                                                    content: "true".to_string(),
                                                    kind: ExpressionKind::LiteralBoolean,
                                                }
                                            },
                                        }],
                                    },
                                },
                            ],
                        },
                    },
                },
            }
        );
    }

    #[test]
    fn call_expressions() {
        assert_eq!(
            parse(r#"addTwo(2,3)"#),
            Statement {
                content: r#"addTwo(2,3)"#.into(),
                kind: StatementKind::ExprStmt {
                    expr: Expression {
                        content: r#"addTwo(2,3)"#.to_string(),
                        kind: ExpressionKind::Call {
                            function: Box::new(Expression {
                                content: "addTwo".to_string(),
                                kind: ExpressionKind::Identifier,
                            }),
                            arguments: vec![
                                Expression {
                                    content: "2".to_string(),
                                    kind: ExpressionKind::LiteralInteger,
                                },
                                Expression {
                                    content: "3".to_string(),
                                    kind: ExpressionKind::LiteralInteger,
                                }
                            ],
                        },
                    },
                },
            }
        );
    }

    #[test]
    fn index_expressions() {
        assert_eq!(
            parse(r#"identifier[2]"#),
            Statement {
                content: r#"identifier[2]"#.into(),
                kind: StatementKind::ExprStmt {
                    expr: Expression {
                        content: r#"identifier[2]"#.to_string(),
                        kind: ExpressionKind::IndexExpression {
                            left: Box::new(Expression {
                                content: "identifier".to_string(),
                                kind: ExpressionKind::Identifier,
                            }),
                            idx: Box::new(Expression {
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
