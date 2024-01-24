use std::ops::Deref;

use crate::parser::StatementKind::EndStatement;
use crate::parser::{
    BinaryOp, Expression, ExpressionKind, Node, Parser, StatementBlock, StatementKind, UnaryOp,
};

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
enum Object {
    Integer(i32),
    Boolean(Booleans),
    Null,
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
enum Booleans {
    True,
    False,
}
fn eval_source(str: &str) -> Object {
    let parser = Parser::from(str);
    let mut program: Vec<StatementKind> = vec![];
    loop {
        let stmt = parser.next_statement();
        if stmt.kind == EndStatement {
            break;
        }

        program.push(stmt.kind)
    }

    return eval_program(program);
}

fn eval(node: Node) -> Object {
    return match node {
        Node::Expr(expr) => eval_expression(expr),
        Node::Stmt(stmt) => eval_statement(stmt),
        Node::Program(v) => eval_program(v),
    };
}

fn eval_program(v: Vec<StatementKind>) -> Object {
    let mut result = Object::Null;
    for e in v {
        result = eval(Node::Stmt(&e))
    }
    result
}

fn eval_statement(stmt: &StatementKind) -> Object {
    match stmt {
        StatementKind::ExprStmt { expr } => eval_expression(&expr.kind),
        StatementKind::LetStmt { .. } => todo!(),
        StatementKind::ReturnStmt { .. } => todo!(),
        StatementKind::IllegalStatement { .. } => todo!(),
        EndStatement => todo!(),
    }
}

fn eval_expression(expr: &ExpressionKind) -> Object {
    match expr {
        ExpressionKind::LiteralInteger { value } => Object::Integer(*value),
        ExpressionKind::LiteralBoolean { value } => as_boolean(*value),
        ExpressionKind::Unary { op, expr } => match (op, &expr.kind) {
            (UnaryOp::OpNot, k) => match eval_expression(k) {
                Object::Boolean(Booleans::True) => Object::Boolean(Booleans::False),
                Object::Boolean(Booleans::False) => Object::Boolean(Booleans::True),
                Object::Null => Object::Boolean(Booleans::False),
                _ => Object::Boolean(Booleans::False),
            },

            (UnaryOp::OpNeg, k) => match eval_expression(k) {
                Object::Integer(value) => Object::Integer(-value),
                _ => Object::Null,
            },
        },
        ExpressionKind::LiteralFunction { .. } => todo!(),
        ExpressionKind::Identifier { .. } => todo!(),
        ExpressionKind::Binary { op, left, right } => {
            match (eval_expression(&left.kind), eval_expression(&right.kind)) {
                (Object::Integer(l), Object::Integer(r)) => eval_binary_int(op, l, r),
                (Object::Boolean(l), Object::Boolean(r)) => eval_binary_bool(op, l, r),
                _ => todo!(),
            }
        }
        ExpressionKind::Conditional { condition, positive, negative } => {
            eval_conditional(condition, positive, negative)
        }

        ExpressionKind::Call { .. } => todo!(),
        ExpressionKind::IllegalExpression { .. } => todo!(),
    }
}

fn eval_conditional(
    condition: &Box<Expression>,
    positive: &StatementBlock,
    negative: &Option<StatementBlock>,
) -> Object {
    match eval_expression(&condition.kind) {
        Object::Boolean(b) => match b {
            Booleans::True => eval_block(positive),
            Booleans::False => match negative {
                None => Object::Null,
                Some(block) => eval_block(block),
            },
        },
        Object::Integer(i) => eval_block(positive),
        Object::Null => Object::Null,
    }
}

fn eval_block(block: &StatementBlock) -> Object {
    let mut result = Object::Null;
    for x in &block.statements {
        result = eval_statement(&x.kind)
    }
    return result;
}

fn eval_binary_bool(op: &BinaryOp, l: Booleans, r: Booleans) -> Object {
    match op {
        BinaryOp::OpEquals => as_boolean(l == r),
        BinaryOp::OpDiffers => as_boolean(l != r),
        _ => Object::Null,
    }
}

fn eval_binary_int(op: &BinaryOp, l: i32, r: i32) -> Object {
    match op {
        BinaryOp::OpPlus => Object::Integer(l + r),
        BinaryOp::OpMinus => Object::Integer(l - r),
        BinaryOp::OpTimes => Object::Integer(l * r),
        BinaryOp::OpDiv => match r {
            0 => Object::Null,
            _ => Object::Integer(l / r),
        },

        BinaryOp::OpGreater => as_boolean(l > r),
        BinaryOp::OpLesser => as_boolean(l < r),
        BinaryOp::OpEquals => as_boolean(l == r),
        BinaryOp::OpDiffers => as_boolean(l != r),
    }
}

fn as_boolean(value: bool) -> Object {
    match value {
        true => Object::Boolean(Booleans::True),
        false => Object::Boolean(Booleans::False),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn literal_int() {
        let input = "10";
        let result = eval_source(input);
        assert_eq!(result, Object::Integer(10))
    }

    #[test]
    fn literal_bool() {
        let input = "true";
        let result = eval_source(input);

        assert_eq!(result, Object::Boolean(Booleans::True));
    }

    #[test]
    fn unaries() {
        assert_eq!(eval_source("!5"), Object::Boolean(Booleans::False));
        assert_eq!(eval_source("!true"), Object::Boolean(Booleans::False));
        assert_eq!(eval_source("!!false"), Object::Boolean(Booleans::False));

        assert_eq!(eval_source("!!5"), Object::Boolean(Booleans::True));
        assert_eq!(eval_source("!false"), Object::Boolean(Booleans::True));
        assert_eq!(eval_source("!!true"), Object::Boolean(Booleans::True));

        assert_eq!(eval_source("-5"), Object::Integer(-5));
        assert_eq!(eval_source("!-5"), Object::Boolean(Booleans::False));
        assert_eq!(eval_source("!!-5"), Object::Boolean(Booleans::True));
        assert_eq!(eval_source("-true"), Object::Null);
    }

    #[test]
    fn int_binaries() {
        assert_eq!(eval_source("5 + 5"), Object::Integer(10));
        assert_eq!(eval_source("5 - 5"), Object::Integer(0));
        assert_eq!(eval_source("5 * 5"), Object::Integer(25));
        assert_eq!(eval_source("5 / 5"), Object::Integer(1));
        assert_eq!(eval_source("5 / 0"), Object::Null);

        assert_eq!(eval_source("5 > 5"), Object::Boolean(Booleans::False));
        assert_eq!(eval_source("5 < 5"), Object::Boolean(Booleans::False));
        assert_eq!(eval_source("5 != 5"), Object::Boolean(Booleans::False));
        assert_eq!(eval_source("5 == 5"), Object::Boolean(Booleans::True));

        assert_eq!(
            eval_source("(5 + 10 * 2 + 15 / 3 ) * 2 + -10"),
            Object::Integer(50)
        );
    }

    #[test]
    fn bool_binaries() {
        assert_eq!(eval_source("true == true"), Object::Boolean(Booleans::True));
        assert_eq!(
            eval_source("true == false"),
            Object::Boolean(Booleans::False)
        );

        assert_eq!(
            eval_source("true != true"),
            Object::Boolean(Booleans::False)
        );
        assert_eq!(
            eval_source("true != false"),
            Object::Boolean(Booleans::True)
        );

        assert_eq!(
            eval_source("(1 < 2) == true"),
            Object::Boolean(Booleans::True)
        );
        assert_eq!(
            eval_source("(1 < 2) == false"),
            Object::Boolean(Booleans::False)
        );

        assert_eq!(
            eval_source("(1 > 2) == true"),
            Object::Boolean(Booleans::False)
        );
        assert_eq!(
            eval_source("(1 > 2) == false"),
            Object::Boolean(Booleans::True)
        );
    }

    #[test]
    fn conditionals() {
        assert_eq!(eval_source("if (true) { 10 }"), Object::Integer(10));
        assert_eq!(eval_source("if (false) { 10 }"), Object::Null);
        assert_eq!(eval_source("if (1) { 10 }"), Object::Integer(10));
        assert_eq!(eval_source("if (1 < 2 ) { 10 }"), Object::Integer(10));
        assert_eq!(eval_source("if (1 > 2 ) { 10 }"), Object::Null);

        assert_eq!(
            eval_source("if (1 > 2 ) { 10 } else { 20 }"),
            Object::Integer(20)
        );
        assert_eq!(
            eval_source("if (1 < 2 ) { 10 } else { 20 }"),
            Object::Integer(10)
        );
    }
}
