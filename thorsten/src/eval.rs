use crate::eval::Object::{False, True};
use crate::parser::StatementKind::EndStatement;
use crate::parser::{ExpressionKind, Node, Parser, StatementKind, UnaryOp};

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
enum Object {
    Integer(i32),
    True,
    False,
    Null,
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

    return eval(Node::Program(program));
}

fn eval(node: Node) -> Object {
    return match node {
        Node::Expr(expr) => match expr {
            ExpressionKind::LiteralInteger { value } => Object::Integer(value),
            ExpressionKind::LiteralBoolean { value } => as_boolean(value),
            ExpressionKind::Unary { op, expr } => match (op, expr.kind) {
                (UnaryOp::OpNot, k) => match eval(Node::Expr(k)) {
                    Object::True => False,
                    Object::False => True,
                    Object::Null => True,
                    _ => False,
                },

                (UnaryOp::OpNeg, k) => match eval(Node::Expr(k)) {
                    Object::Integer(value) => Object::Integer(-value),
                    _ => Object::Null,
                },
            },
            ExpressionKind::LiteralFunction { .. } => todo!(),
            ExpressionKind::Identifier { .. } => todo!(),
            ExpressionKind::Binary { .. } => todo!(),
            ExpressionKind::Conditional { .. } => todo!(),
            ExpressionKind::Call { .. } => todo!(),
            ExpressionKind::IllegalExpression { .. } => todo!(),
        },

        Node::Stmt(stmt) => match stmt {
            StatementKind::ExprStmt { expr } => eval(Node::Expr(expr.kind)),
            StatementKind::LetStmt { .. } => todo!(),
            StatementKind::ReturnStmt { .. } => todo!(),
            StatementKind::IllegalStatement { .. } => todo!(),
            EndStatement => todo!(),
        },
        Node::Program(v) => {
            let mut result = Object::Null;
            for e in v {
                result = eval(Node::Stmt(e))
            }
            return result;
        }
    };
}

fn as_boolean(value: bool) -> Object {
    match value {
        true => Object::True,
        false => Object::False,
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

        assert_eq!(result, Object::True);
    }

    #[test]
    fn unaries() {
        assert_eq!(eval_source("!5"), Object::False);
        assert_eq!(eval_source("!true"), Object::False);
        assert_eq!(eval_source("!!false"), Object::False);

        assert_eq!(eval_source("!!5"), Object::True);
        assert_eq!(eval_source("!false"), Object::True);
        assert_eq!(eval_source("!!true"), Object::True);

        assert_eq!(eval_source("-5"), Object::Integer(-5));
    }
}
