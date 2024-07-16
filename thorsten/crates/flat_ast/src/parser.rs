use crate::{
    lexer::{RawToken, token_after, TokenKind},
    range::Range,
};

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

#[derive(Debug)]
pub enum RawStatementKind {
    LetStmt(LetStmt),
    ReturnStmt(ReturnStmt),
    ExprStmt(ExprStmt),
    IllegalStatement,
    EndStatement,
}

#[derive(Debug)]
pub enum RawExpressionKind {
    LiteralInteger,
    LiteralString,
    LiteralBoolean,
    Parenthesized { expr: ExpressionId },
    LiteralFunction(LiteralFunction),
    LiteralArray(LiteralArray),
    LiteralHash(LiteralHash),
    Identifier,
    Unary(Unary),
    Binary(Binary),
    Conditional(Conditional),
    Call(Call),
    IndexExpression(IndexExpression),
    IllegalExpression,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Ord, PartialOrd)]
pub enum UnaryOp { OpNot, OpNeg }

#[derive(Debug, PartialEq, Eq, Clone, Copy, Ord, PartialOrd)]
pub enum BinaryOp { Plus, Minus, Times, Div, Greater, Lesser, Equals, Differs }

#[derive(Clone, Copy, PartialOrd, PartialEq)]
enum Precedence { Lowest, Equals, LesserGreater, Sum, Product, Prefix, Apply, Index }

#[derive(Debug)]
pub struct LiteralFunction {
    pub parameters: Vec<ExpressionId>,
    pub body: Vec<StatementId>,
}

#[derive(Debug)]
pub struct LiteralArray {
    pub values: Vec<ExpressionId>,
}

#[derive(Debug)]
pub struct LiteralHash {
    pub values: Vec<(ExpressionId, ExpressionId)>,
}

#[derive(Debug)]
pub struct Unary {
    pub op: UnaryOp,
    pub expr: ExpressionId,
}

#[derive(Debug)]
pub struct Binary {
    pub op: BinaryOp,
    pub left: ExpressionId,
    pub right: ExpressionId,
}

#[derive(Debug)]
pub struct Conditional {
    pub condition: ExpressionId,
    pub positive: Vec<StatementId>,
    pub negative: Vec<StatementId>,
}

#[derive(Debug)]
pub struct Call {
    pub function: ExpressionId,
    pub arguments: Vec<ExpressionId>,
}

#[derive(Debug)]
pub struct IndexExpression {
    pub left: ExpressionId,
    pub idx: ExpressionId,
}

#[derive(Debug)]
pub struct LetStmt {
    pub name: ExpressionId,
    pub expr: ExpressionId,
}

#[derive(Debug)]
pub struct ReturnStmt {
    pub expr: ExpressionId,
}

#[derive(Debug)]
pub struct ExprStmt {
    pub expr: ExpressionId,
}

impl From<&TokenKind> for Precedence {
    fn from(value: &TokenKind) -> Self {
        match value {
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

pub(crate) struct ExprTable {
    pub(crate) statements: Vec<RawStatement>,
    pub(crate) expressions: Vec<RawExpression>,
}

impl ExprTable {
    pub(crate) fn new() -> ExprTable {
        ExprTable {
            statements: Vec::new(),
            expressions: Vec::new(),
        }
    }
    fn add_statement(&mut self, statement: RawStatement) -> StatementId {
        let id = self.statements.len();
        self.statements.push(statement);
        StatementId(id)
    }

    fn add_expression(&mut self, expression: RawExpression) -> ExpressionId {
        let id = self.expressions.len();
        self.expressions.push(expression);
        ExpressionId(id)
    }

    pub(crate) fn get_statement(&self, id: &StatementId) -> &RawStatement {
        self.statements.get(id.0).unwrap()
    }

    pub(crate) fn get_expression(&self, id: &ExpressionId) -> &RawExpression {
        self.expressions.get(id.0).unwrap()
    }
}

pub(crate) struct CompUnit<'a> {
    pub source: &'a str,
    table: ExprTable,
    pub statements: Vec<StatementId>,
}

pub(crate) fn unit(source: &str) -> CompUnit {
    let mut statements = Vec::new();
    let table = {
        let mut is_end = false;
        let mut table = ExprTable::new();
        let mut current = Range::new(0, 0);

        while !is_end {
            let statement = statement_after(source, &current, &mut table);

            current = statement.range;
            is_end = matches!(statement.kind, RawStatementKind::EndStatement);
            statements.push(table.add_statement(statement));
        }

        table
    };

    CompUnit { source, table, statements }
}

impl<'a> CompUnit<'_> {
    pub fn text(&self, range: &Range) -> &str {
        &self.source[range]
    }

    pub fn bool(&self, range: &Range) -> Result<bool, std::str::ParseBoolError> {
        self.source[range].parse::<bool>()
    }
    pub fn int(&self, range: &Range) -> Result<i64, std::num::ParseIntError> {
        self.source[range].parse::<i64>()
    }

    pub fn statement(&self, id: StatementId) -> &RawStatement {
        &self.table.statements.get(id.0).unwrap()
    }
    pub fn expression(&self, id: ExpressionId) -> &RawExpression {
        &self.table.expressions.get(id.0).unwrap()
    }
}

pub(crate) fn raw_statements(input: &str, table: &mut ExprTable) -> Vec<RawStatement> {
    let mut current = Range::new(0, 0);
    let iter = std::iter::from_fn(move || {
        let statement = statement_after(input, &current, table);
        current = statement.range;
        match statement {
            RawStatement { kind: RawStatementKind::EndStatement, .. } => None,
            s => Some(s),
        }
    });
    Vec::from_iter(iter)
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

        RawToken { kind: TokenKind::If, range } => conditional_expression(input, &range, table),

        RawToken { kind: TokenKind::Function, range } => {
            function_expression(input, &range, table)
        }

        RawToken { kind, range } if matches!(kind, TokenKind::Bang | TokenKind::Minus) => {
            unary_expression(input, &range, kind, table)
        }

        RawToken { kind: TokenKind::Lparen, .. } => parenthesized_expression(input, range, table),
        RawToken { kind: TokenKind::Lbrace, .. } => hash_expression(input, range, table),
        RawToken { kind: TokenKind::LBracket, .. } => array_expression(input, range, table),

        RawToken { range, .. } => RawExpression {
            range,
            kind: RawExpressionKind::IllegalExpression,
        },
    };

    loop {
        let RawToken { kind, .. } = token_after(input, &left_expr.range);
        let next_precedence = Precedence::from(&kind);
        if precedence >= next_precedence {
            break;
        }

        left_expr = match kind {
            TokenKind::Lparen => call_expression(input, left_expr, table),
            TokenKind::LBracket => index_expression(input, left_expr, table),

            TokenKind::Plus => binary_expression(input, left_expr, table, next_precedence),
            TokenKind::Minus => binary_expression(input, left_expr, table, next_precedence),
            TokenKind::Asterisk => binary_expression(input, left_expr, table, next_precedence),
            TokenKind::Slash => binary_expression(input, left_expr, table, next_precedence),
            TokenKind::Lt => binary_expression(input, left_expr, table, next_precedence),
            TokenKind::Gt => binary_expression(input, left_expr, table, next_precedence),
            TokenKind::Equals => binary_expression(input, left_expr, table, next_precedence),
            TokenKind::Differs => binary_expression(input, left_expr, table, next_precedence),

            _ => left_expr,
        };
    }

    left_expr
}

fn statement_after(input: &str, start: &Range, table: &mut ExprTable) -> RawStatement {
    let statement = match token_after(input, start) {
        RawToken { kind: TokenKind::Return, range } => return_statement(input, &range, table),
        RawToken { kind: TokenKind::Let, range } => let_statement(input, &range, table),
        RawToken { kind: TokenKind::Eof, range } => end_statement(input, &range, table),
        _ => expression_statement(input, start, table),
    };

    match token_after(input, &statement.range) {
        RawToken { kind: TokenKind::Semicolon, range } => RawStatement {
            range: Range::merge(&statement.range, &range),
            ..statement
        },
        _ => statement,
    }
}

fn binary_expression(
    input: &str,
    left: RawExpression,
    table: &mut ExprTable,
    next_precedence: Precedence,
) -> RawExpression {
    let RawToken { kind, range } = token_after(input, &left.range);
    let op = match kind {
        TokenKind::Plus => BinaryOp::Plus,
        TokenKind::Minus => BinaryOp::Minus,
        TokenKind::Asterisk => BinaryOp::Times,
        TokenKind::Slash => BinaryOp::Div,
        TokenKind::Lt => BinaryOp::Lesser,
        TokenKind::Gt => BinaryOp::Greater,
        TokenKind::Equals => BinaryOp::Equals,
        TokenKind::Differs => BinaryOp::Differs,
        _ => {
            return RawExpression {
                range: Range::merge(&left.range, &range),
                kind: RawExpressionKind::IllegalExpression,
            }
        }
    };

    let right = expression_after(input, &range, table, next_precedence);
    RawExpression {
        range: Range::merge(&left.range, &right.range),
        kind: RawExpressionKind::Binary(Binary {
            op,
            left: table.add_expression(left),
            right: table.add_expression(right),
        }),
    }
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
        kind: RawExpressionKind::IndexExpression(IndexExpression {
            left: table.add_expression(left),
            idx: table.add_expression(expr),
        }),
    }
}

fn call_expression(input: &str, left: RawExpression, table: &mut ExprTable) -> RawExpression {
    let left_paren = match expect_token(input, &left.range, &left.range, TokenKind::Lparen) {
        Ok(t) => t,
        Err(e) => return e,
    };
    let ExpressionList { range, expressions } =
        expression_list(input, &left_paren.range, table, TokenKind::Rparen);

    let right_paren = match expect_token(input, &left.range, &range, TokenKind::Rparen) {
        Ok(t) => t,
        Err(e) => return e,
    };

    RawExpression {
        range: Range::merge(&left.range, &right_paren.range),
        kind: RawExpressionKind::Call(Call {
            function: table.add_expression(left),
            arguments: expressions,
        }),
    }
}

fn array_expression(input: &str, start: &Range, table: &mut ExprTable) -> RawExpression {
    let left_brace = match expect_token(input, start, start, TokenKind::LBracket) {
        Ok(token) => token,
        Err(expr) => return expr,
    };

    let ExpressionList { range, expressions } =
        expression_list(input, &left_brace.range, table, TokenKind::RBracket);

    let right_brace = match expect_token(input, start, &range, TokenKind::RBracket) {
        Ok(token) => token,
        Err(expr) => return expr,
    };

    RawExpression {
        range: Range::merge(start, &right_brace.range),
        kind: RawExpressionKind::LiteralArray(LiteralArray { values: expressions }),
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
        let value = match expect_token(input, start, &key.range, TokenKind::Colon) {
            Err(e) => return e,
            Ok(RawToken { range, .. }) => expression_after(input, &range, table, Precedence::Lowest)
        };

        if let RawToken { kind: TokenKind::Comma, range } = token_after(input, &value.range) {
            current = range
        } else {
            current = value.range;
        }

        values.push((table.add_expression(key), table.add_expression(value)));
    }

    RawExpression {
        range: Range::merge(start, &current),
        kind: RawExpressionKind::LiteralHash(LiteralHash { values }),
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

    let StatementBlock { statements, range } =
        match statement_block(input, &right_paren.range, table) {
            Ok(block) => block,
            Err(e) => return e,
        };

    RawExpression {
        range: Range::merge(start, &range),
        kind: RawExpressionKind::LiteralFunction(LiteralFunction { parameters: expressions, body: statements }),
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

    let StatementBlock { statements, range } = {
        let RawToken { kind, range } = token_after(input, &positive.range);
        if kind != TokenKind::Else {
            StatementBlock { statements: vec![], range: positive.range }
        } else {
            match statement_block(input, &range, table) {
                Ok(t) => t,
                Err(expr) => return expr,
            }
        }
    };

    RawExpression {
        range: Range::merge(start, &range),
        kind: RawExpressionKind::Conditional(Conditional {
            condition: table.add_expression(condition),
            positive: positive.statements,
            negative: statements,
        }),
    }
}

fn parenthesized_expression(input: &str, start: &Range, table: &mut ExprTable) -> RawExpression {
    let left_paren = match expect_token(input, start, start, TokenKind::Lparen) {
        Ok(token) => token,
        Err(expr) => return expr,
    };

    let expr = expression_after(input, &left_paren.range, table, Precedence::Lowest);

    let right_paren = match expect_token(input, start, &expr.range, TokenKind::Rparen) {
        Ok(token) => token,
        Err(expr) => return expr,
    };

    RawExpression {
        range: Range::merge(start, &right_paren.range),
        kind: RawExpressionKind::Parenthesized { expr: table.add_expression(expr) },
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
        kind: RawExpressionKind::Unary(Unary { op, expr: table.add_expression(expr) }),
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
    RawStatement {
        range: Range::merge(start, &expr.range),
        kind: RawStatementKind::LetStmt(LetStmt {
            name: table.add_expression(ident),
            expr: table.add_expression(expr),
        }),
    }
}

fn return_statement(input: &str, start: &Range, table: &mut ExprTable) -> RawStatement {
    let expression = expression_after(input, start, table, Precedence::Lowest);
    RawStatement {
        range: Range::merge(start, &expression.range),
        kind: RawStatementKind::ReturnStmt(ReturnStmt { expr: table.add_expression(expression) }),
    }
}

fn end_statement(_input: &str, start: &Range, _table: &mut ExprTable) -> RawStatement {
    RawStatement {
        range: Range::merge(start, start),
        kind: RawStatementKind::EndStatement,
    }
}

fn expression_statement(input: &str, start: &Range, table: &mut ExprTable) -> RawStatement {
    let expr = expression_after(input, start, table, Precedence::Lowest);
    RawStatement {
        range: expr.range,
        kind: RawStatementKind::ExprStmt(ExprStmt { expr: table.add_expression(expr) }),
    }
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

    Ok(StatementBlock {
        range: Range::merge(start, &right_brace.range),
        statements: res,
    })
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
        RawToken { kind: _kind, range } => Err(RawExpression {
            range: Range::merge(start, &range),
            kind: RawExpressionKind::IllegalExpression,
        }),
    }
}

struct StatementBlock {
    statements: Vec<StatementId>,
    range: Range,
}

struct ExpressionList {
    range: Range,
    expressions: Vec<ExpressionId>,
}

#[cfg(test)]
mod tests {
    use crate::{
        parser::{
            BinaryOp, ExpressionId, ExprTable, RawExpression, RawExpressionKind, RawStatement,
            RawStatementKind, statement_after, StatementId, UnaryOp,
        },
        range::Range,
    };
    use crate::parser::{ExprStmt, LetStmt, ReturnStmt};

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
        Parenthesized {
            expression: Box<Expression>,
        },
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

        Statement {
            content: input[statement.range].into(),
            kind: match statement.kind {
                RawStatementKind::EndStatement => StatementKind::EndStatement {},
                RawStatementKind::IllegalStatement => StatementKind::IllegalStatement {},
                RawStatementKind::LetStmt(stmt) => StatementKind::LetStmt {
                    name: lookup_expression(input, stmt.name, &table),
                    expr: lookup_expression(input, stmt.expr, &table),
                },
                RawStatementKind::ReturnStmt(stmt) => StatementKind::ReturnStmt {
                    expr: lookup_expression(input, stmt.expr, &table)
                },
                RawStatementKind::ExprStmt(stmt) => StatementKind::ExprStmt {
                    expr: lookup_expression(input, stmt.expr, &table)
                }
            },
        }
    }

    fn lookup_expression(input: &str, id: ExpressionId, table: &ExprTable) -> Expression {
        let RawExpression { range, kind } = table.get_expression(&id);
        Expression {
            content: input[range.start..range.end].into(),
            kind: match kind {
                RawExpressionKind::LiteralInteger => ExpressionKind::LiteralInteger,
                RawExpressionKind::LiteralString => ExpressionKind::LiteralString,
                RawExpressionKind::LiteralBoolean => ExpressionKind::LiteralBoolean,
                RawExpressionKind::Identifier => ExpressionKind::Identifier,
                RawExpressionKind::IllegalExpression => ExpressionKind::IllegalExpression,
                RawExpressionKind::LiteralFunction(function) => ExpressionKind::LiteralFunction {
                    parameters: function.parameters
                        .iter()
                        .map(|p| lookup_expression(input, *p, table))
                        .map(|e| e.content)
                        .collect(),
                    body: function.body
                        .iter()
                        .map(|p| lookup_statement(input, *p, table))
                        .collect(),
                },
                RawExpressionKind::LiteralArray(array) => ExpressionKind::LiteralArray {
                    values: array.values
                        .iter()
                        .map(|p| lookup_expression(input, *p, table))
                        .collect(),
                },
                RawExpressionKind::LiteralHash(hash) => ExpressionKind::LiteralHash {
                    values: hash.values
                        .iter()
                        .map(|(k, v)| {
                            (
                                lookup_expression(input, *k, table),
                                lookup_expression(input, *v, table),
                            )
                        })
                        .collect(),
                },
                RawExpressionKind::Unary(unary) => ExpressionKind::Unary {
                    op: unary.op,
                    expr: lookup_expression(input, unary.expr, table).into(),
                },
                RawExpressionKind::Binary(binary) => ExpressionKind::Binary {
                    op: binary.op,
                    left: lookup_expression(input, binary.left, table).into(),
                    right: lookup_expression(input, binary.right, table).into(),
                },
                RawExpressionKind::Conditional(cond) => ExpressionKind::Conditional {
                    condition: lookup_expression(input, cond.condition, table).into(),
                    positive: cond.positive
                            .iter()
                            .map(|p| lookup_statement(input, *p, table))
                            .collect(),
                    negative: cond.negative
                            .iter()
                            .map(|p| lookup_statement(input, *p, table))
                            .collect(),
                },
                RawExpressionKind::Call(call) => ExpressionKind::Call {
                    function: lookup_expression(input, call.function, table).into(),
                    arguments: call.arguments
                        .iter()
                        .map(|a| lookup_expression(input, *a, table))
                        .collect(),
                },
                RawExpressionKind::IndexExpression(index) => ExpressionKind::IndexExpression {
                    left: lookup_expression(input, index.left, table).into(),
                    idx: lookup_expression(input, index.idx, table).into(),
                },
                RawExpressionKind::Parenthesized { expr } => ExpressionKind::Parenthesized {
                    expression: lookup_expression(input, *expr, table).into(),
                },
            },
        }
    }

    fn lookup_statement(input: &str, id: StatementId, table: &ExprTable) -> Statement {
        let RawStatement { range, kind } = table.get_statement(&id);
        Statement {
            content: input[range.start..range.end].into(),
            kind: match kind {
                RawStatementKind::LetStmt(LetStmt { name, expr }) => StatementKind::LetStmt {
                    name: lookup_expression(input, *name, table),
                    expr: lookup_expression(input, *expr, table),
                },
                RawStatementKind::ReturnStmt(ReturnStmt { expr }) => {
                    StatementKind::ReturnStmt { expr: lookup_expression(input, *expr, table) }
                }
                RawStatementKind::ExprStmt(ExprStmt { expr }) => {
                    StatementKind::ExprStmt { expr: lookup_expression(input, *expr, table) }
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
                        kind: ExpressionKind::Parenthesized {
                            expression: Box::new(Expression {
                                content: "-2".to_string(),
                                kind: ExpressionKind::Unary {
                                    op: UnaryOp::OpNeg,
                                    expr: Box::new(Expression {
                                        content: "2".to_string(),
                                        kind: ExpressionKind::LiteralInteger,
                                    }),
                                }
                            })
                        }
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

    #[test]
    fn binary_expressions() {
        assert_eq!(
            parse(r#"2 + 3"#),
            Statement {
                content: r#"2 + 3"#.into(),
                kind: StatementKind::ExprStmt {
                    expr: Expression {
                        content: r#"2 + 3"#.to_string(),
                        kind: ExpressionKind::Binary {
                            op: BinaryOp::Plus,
                            left: Box::new(Expression {
                                content: "2".to_string(),
                                kind: ExpressionKind::LiteralInteger,
                            }),
                            right: Box::new(Expression {
                                content: "3".to_string(),
                                kind: ExpressionKind::LiteralInteger,
                            }),
                        },
                    },
                },
            }
        );
    }
}
