use std::collections::HashMap;
use std::ops::Deref;

use crate::eval::Object::Error;
use crate::parser::{
    BinaryOp, Expression, ExpressionKind, Parser, StatementBlock, StatementKind, UnaryOp,
};
use crate::parser::StatementKind::EndStatement;

#[derive(Debug, Eq, PartialEq, Clone)]
enum Object {
    Integer(i32),
    Boolean(Booleans),
    Null,
    Return(Box<Object>),
    Error(String),
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
enum Booleans {
    True,
    False,
}

struct VM {
    env: Environment,
}

struct Environment {
    bindings: HashMap<String, Object>,
}

impl Environment {
    pub fn add(&mut self, name: &str, expr: Object) {
        self.bindings.insert(name.to_owned(), expr);
    }
    pub fn get(&mut self, name: &str) -> Option<&Object> {
        self.bindings.get(name)
    }
}

impl VM {
    fn new() -> VM {
        return VM {
            env: Environment {
                bindings: HashMap::<String, Object>::new()
            }
        }
    }
    fn eval_source(&mut self, str: &str) -> Object {
        let parser = Parser::from(str);
        let mut program: Vec<StatementKind> = vec![];
        loop {
            let stmt = parser.next_statement();
            if stmt.kind == EndStatement {
                break;
            }

            program.push(stmt.kind)
        }

        return self.eval_program(program);
    }

    fn eval_program(&mut self, v: Vec<StatementKind>) -> Object {
        let mut result = Object::Null;
        for e in v {
            result = match self.eval_statement(&e) {
                Object::Return(ret) => return *ret,
                error @ Object::Error { .. } => return error,
                other => other,
            }
        }
        result
    }

    fn eval_statement(&mut self, stmt: &StatementKind) -> Object {
        match stmt {
            StatementKind::ExprStmt { expr } => self.eval_expression(&expr.kind),
            StatementKind::ReturnStmt { expr } => {
                Object::Return(Box::from(self.eval_expression(&expr.kind)))
            }
            StatementKind::LetStmt { name, expr } => {
                match self.eval_expression(&expr.kind) {
                    e @ Error(_) => e,
                    Object::Return(_) => Error("let return is not valid".to_owned()),
                    obj => {
                        self.env.add(name, obj.clone());
                        obj
                    }
                }
            },
            StatementKind::IllegalStatement { expr } => Error(format!("Illegal Statement: {:?}", expr)),
            EndStatement => todo!(),
        }
    }

    fn eval_expression(&mut self, expr: &ExpressionKind) -> Object {
        match expr {
            ExpressionKind::LiteralInteger { value } => Object::Integer(*value),
            ExpressionKind::LiteralBoolean { value } => as_boolean(*value),
            ExpressionKind::Unary { op, expr } => match (op, &expr.kind) {
                (UnaryOp::OpNot, k) => match self.eval_expression(k) {
                    Object::Boolean(Booleans::True) => Object::Boolean(Booleans::False),
                    Object::Boolean(Booleans::False) => Object::Boolean(Booleans::True),
                    Object::Null => Object::Boolean(Booleans::False),
                    _ => Object::Boolean(Booleans::False),
                },

                (UnaryOp::OpNeg, k) => match self.eval_expression(k) {
                    Object::Integer(value) => Object::Integer(-value),
                    obj => Error(format!("Unknown operator: -{:?}", obj)),
                },
            },
            ExpressionKind::LiteralFunction { .. } => todo!(),
            ExpressionKind::Identifier { name } => match self.env.get(name) {
                None => Error(format!("Unknown identifier {:?}", name)),
                Some(expr) => expr.clone()
            }

            ExpressionKind::Binary { op, left, right } => {
                match (self.eval_expression(&left.kind), self.eval_expression(&right.kind)) {
                    (Object::Integer(l), Object::Integer(r)) => self.eval_binary_int(op, l, r),
                    (Object::Boolean(l), Object::Boolean(r)) => self.eval_binary_bool(op, l, r),
                    (l, r) => Error(format!("Unknown operator: {:?} {:?} {:?}", l, op, r)),
                }
            }
            e @ ExpressionKind::Conditional { condition, positive, negative } => {
                self.eval_conditional(condition, positive, negative)
            }

            ExpressionKind::Call { .. } => todo!(),
            ExpressionKind::IllegalExpression { .. } => todo!(),
        }
    }

    fn eval_conditional(
        &mut self,
        condition: &Box<Expression>,
        positive: &StatementBlock,
        negative: &Option<StatementBlock>,
    ) -> Object {
        match self.eval_expression(&condition.kind) {
            Object::Boolean(b) => match b {
                Booleans::True => self.eval_block(positive),
                Booleans::False => match negative {
                    None => Object::Null,
                    Some(block) => self.eval_block(block),
                },
            },
            Object::Integer(i) => self.eval_block(positive),
            Object::Error(m) => Error(m),
            _ => Object::Null,
        }
    }

    fn eval_block(&mut self, block: &StatementBlock) -> Object {
        let mut result = Object::Null;
        for x in &block.statements {
            result = self.eval_statement(&x.kind);
            if matches!(result, Object::Return(_)) {
                return result;
            }
            if matches!(result, Object::Error(_)) {
                return result;
            }
        }
        return result;
    }

    fn eval_binary_bool(&mut self, op: &BinaryOp, l: Booleans, r: Booleans) -> Object {
        match op {
            BinaryOp::OpEquals => as_boolean(l == r),
            BinaryOp::OpDiffers => as_boolean(l != r),
            op => Error(format!("Unknown operator: {:?} {:?} {:?}", l, op, r)),
        }
    }

    fn eval_binary_int(&mut self, op: &BinaryOp, l: i32, r: i32) -> Object {
        match op {
            BinaryOp::OpPlus => Object::Integer(l + r),
            BinaryOp::OpMinus => Object::Integer(l - r),
            BinaryOp::OpTimes => Object::Integer(l * r),
            BinaryOp::OpDiv => match r {
                0 => Object::Error("Cannot divide by 0".to_owned()),
                _ => Object::Integer(l / r),
            },

            BinaryOp::OpGreater => as_boolean(l > r),
            BinaryOp::OpLesser => as_boolean(l < r),
            BinaryOp::OpEquals => as_boolean(l == r),
            BinaryOp::OpDiffers => as_boolean(l != r),
        }
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
        let mut vm = VM::new();
        let input = "10";
        let result = vm.eval_source(input);
        assert_eq!(result, Object::Integer(10))
    }

    #[test]
    fn literal_bool() {
        let mut vm = VM::new();
        let input = "true";
        let result = vm.eval_source(input);

        assert_eq!(result, Object::Boolean(Booleans::True));
    }

    #[test]
    fn unaries() {
        let mut vm = VM::new();
        assert_eq!(vm.eval_source("!5"), Object::Boolean(Booleans::False));
        assert_eq!(vm.eval_source("!true"), Object::Boolean(Booleans::False));
        assert_eq!(vm.eval_source("!!false"), Object::Boolean(Booleans::False));

        assert_eq!(vm.eval_source("!!5"), Object::Boolean(Booleans::True));
        assert_eq!(vm.eval_source("!false"), Object::Boolean(Booleans::True));
        assert_eq!(vm.eval_source("!!true"), Object::Boolean(Booleans::True));

        assert_eq!(vm.eval_source("-5"), Object::Integer(-5));
        assert_eq!(vm.eval_source("!-5"), Object::Boolean(Booleans::False));
        assert_eq!(vm.eval_source("!!-5"), Object::Boolean(Booleans::True));
        assert_eq!(
            vm.eval_source("-true"),
            Error("Unknown operator: -Boolean(True)".to_owned())
        );
    }

    #[test]
    fn int_binaries() {
        let mut vm = VM::new();
        assert_eq!(vm.eval_source("5 + 5"), Object::Integer(10));
        assert_eq!(vm.eval_source("5 - 5"), Object::Integer(0));
        assert_eq!(vm.eval_source("5 * 5"), Object::Integer(25));
        assert_eq!(vm.eval_source("5 / 5"), Object::Integer(1));
        assert_eq!(vm.eval_source("5 / 0"), Error("Cannot divide by 0".to_owned()));

        assert_eq!(vm.eval_source("5 > 5"), Object::Boolean(Booleans::False));
        assert_eq!(vm.eval_source("5 < 5"), Object::Boolean(Booleans::False));
        assert_eq!(vm.eval_source("5 != 5"), Object::Boolean(Booleans::False));
        assert_eq!(vm.eval_source("5 == 5"), Object::Boolean(Booleans::True));

        assert_eq!(
            vm.eval_source("(5 + 10 * 2 + 15 / 3 ) * 2 + -10"),
            Object::Integer(50)
        );
    }

    #[test]
    fn bool_binaries() {
        let mut vm = VM::new();
        assert_eq!(vm.eval_source("true == true"), Object::Boolean(Booleans::True));
        assert_eq!(
            vm.eval_source("true == false"),
            Object::Boolean(Booleans::False)
        );

        assert_eq!(
            vm.eval_source("true != true"),
            Object::Boolean(Booleans::False)
        );
        assert_eq!(
            vm.eval_source("true != false"),
            Object::Boolean(Booleans::True)
        );

        assert_eq!(
            vm.eval_source("(1 < 2) == true"),
            Object::Boolean(Booleans::True)
        );
        assert_eq!(
            vm.eval_source("(1 < 2) == false"),
            Object::Boolean(Booleans::False)
        );

        assert_eq!(
            vm.eval_source("(1 > 2) == true"),
            Object::Boolean(Booleans::False)
        );
        assert_eq!(
            vm.eval_source("(1 > 2) == false"),
            Object::Boolean(Booleans::True)
        );
    }

    #[test]
    fn conditionals() {
        let mut vm = VM::new();
        assert_eq!(vm.eval_source("if (true) { 10 }"), Object::Integer(10));
        assert_eq!(vm.eval_source("if (false) { 10 }"), Object::Null);
        assert_eq!(vm.eval_source("if (1) { 10 }"), Object::Integer(10));
        assert_eq!(vm.eval_source("if (1 < 2 ) { 10 }"), Object::Integer(10));
        assert_eq!(vm.eval_source("if (1 > 2 ) { 10 }"), Object::Null);

        assert_eq!(
            vm.eval_source("if (1 > 2 ) { 10 } else { 20 }"),
            Object::Integer(20)
        );
        assert_eq!(
            vm.eval_source("if (1 < 2 ) { 10 } else { 20 }"),
            Object::Integer(10)
        );
    }

    #[test]
    fn returns() {
        let mut vm = VM::new();
        assert_eq!(vm.eval_source("9; return 2 * 5; 9;"), Object::Integer(10));

        assert_eq!(
            vm.eval_source(
                "
        if (10 > 1) {
            if (10 > 1) {
                return 10;
            }

            return 1;
        }"
            ),
            Object::Integer(10)
        );
    }

    #[test]
    fn errors() {
        let mut vm = VM::new();
        assert_eq!(
            vm.eval_source("5 + true"),
            Object::Error("Unknown operator: Integer(5) OpPlus Boolean(True)".to_owned())
        );
        assert_eq!(
            vm.eval_source("5 + true; 5"),
            Object::Error("Unknown operator: Integer(5) OpPlus Boolean(True)".to_owned())
        );
        assert_eq!(
            vm.eval_source("-true"),
            Object::Error("Unknown operator: -Boolean(True)".to_owned())
        );
        assert_eq!(
            vm.eval_source(
                "if (10 > 1) {
                                        if (10 > 1) {
                                            return true * false;
                                        }
                                        return 1;
                                    }"
            ),
            Object::Error("Unknown operator: True OpTimes False".to_owned())
        );
    }

    #[test]
    fn bindings() {
        let mut vm = VM::new();
        assert_eq!(
            vm.eval_source("let a = 5; a;"),
            Object::Integer(5)
        );
        assert_eq!(
            vm.eval_source("let a = 5 * 5; a;"),
            Object::Integer(25)
        );
        assert_eq!(
            vm.eval_source("let a = 5; let b = a; b"),
            Object::Integer(5)
        );
        assert_eq!(
            vm.eval_source("let a = 5; let b = a; let c = a + b + 5; c;"),
            Object::Integer(15)
        );
        assert_eq!(
            vm.eval_source("foobar"),
            Object::Error("Unknown identifier \"foobar\"".to_owned())
        );
    }

}
