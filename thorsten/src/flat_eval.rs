use std::collections::BTreeMap;
use std::iter::zip;

use crate::flat_lexer::Range;
use crate::flat_parser::UnaryOp::{self, OpNeg, OpNot};
use crate::flat_parser::{
    BinaryOp, ExpressionId, RawExpression, RawExpressionKind, RawStatement, RawStatementKind,
    StatementId,
};

use crate::flat_parser::{raw_statements, ExprTable};
type Env = BTreeMap<String, Object>;

#[derive(Debug, PartialEq, PartialOrd, Eq, Ord, Clone)]
enum Object {
    Null,
    Integer(i64),
    Text(String),
    Boolean(bool),
    List(Vec<Object>),
    Map(BTreeMap<Object, Object>),
    Function(Function),
}

#[derive(Debug, PartialEq, PartialOrd, Eq, Ord, Clone)]
struct Function {
    parameters: Vec<String>,
    body: Vec<StatementId>,
    env: Env,
}
struct VM<'a> {
    table: ExprTable,
    input: &'a str,
}

fn eval(input: &str) -> Object {
    let mut table = ExprTable {
        statements: Vec::new(),
        expressions: Vec::new(),
    };
    let statements = raw_statements(input, &mut table);

    let vm = VM { table, input };
    let mut env = BTreeMap::new();
    let mut result = Object::Null;
    for statement in statements {
        result = vm.eval_statement(&statement, &mut env);
    }
    result
}

impl VM<'_> {
    fn eval_statement(&self, statement: &RawStatement, env: &mut Env) -> Object {
        match statement.kind {
            RawStatementKind::ExprStmt { ref expr } => self.eval_expr(expr, env),
            RawStatementKind::ReturnStmt { ref expr } => self.eval_expr(expr, env),
            RawStatementKind::LetStmt { ref name, ref expr } => {
                let RawExpression { ref range, .. } = self.table.get_expression(name);
                let name = &self.input[range.start..range.end];
                let value = self.eval_expr(expr, env);
                env.insert(name.to_string(), value);
                Object::Null
            }

            RawStatementKind::EndStatement => todo!(),
            RawStatementKind::IllegalStatement => todo!(),
        }
    }

    fn eval_expr(&self, expr: &ExpressionId, env: &mut Env) -> Object {
        let RawExpression { ref range, kind } = self.table.get_expression(expr);

        match kind {
            RawExpressionKind::LiteralString => self.eval_text(range),
            RawExpressionKind::LiteralInteger => self.eval_integer(range),
            RawExpressionKind::LiteralBoolean => self.eval_bool(range),
            RawExpressionKind::LiteralArray { ref values } => self.eval_list(values, env),
            RawExpressionKind::LiteralHash { ref values } => self.eval_map(values, env),
            RawExpressionKind::Identifier => self.eval_identifier(range, env),
            RawExpressionKind::LiteralFunction { parameters, body } => {
                self.eval_function(range, parameters, body, env)
            }

            RawExpressionKind::Parenthesized { ref expr } => self.eval_expr(expr, env),
            RawExpressionKind::Unary { op, ref expr } => self.eval_unary(*op, expr, env),
            RawExpressionKind::Binary { op, ref left, ref right } => {
                self.eval_binary(*op, left, right, env)
            }

            RawExpressionKind::Conditional { ref condition, positive, negative } => {
                self.eval_conditional(condition, positive, negative, env)
            }
            RawExpressionKind::Call { ref function, arguments } => {
                self.eval_call(function, arguments, env)
            }
            RawExpressionKind::IndexExpression { .. } => todo!(),
            RawExpressionKind::IllegalExpression => todo!(),
        }
    }

    fn eval_integer(&self, range: &Range) -> Object {
        self.input[range.start..range.end]
            .parse::<i64>()
            .map(|i| Object::Integer(i))
            .unwrap_or(Object::Integer(0))
    }

    fn eval_text(&self, range: &Range) -> Object {
        let unquoted = (range.start + 1)..(range.end - 1);
        let content = self.input[unquoted].to_string();
        Object::Text(content)
    }

    fn eval_bool(&self, range: &Range) -> Object {
        match &self.input[range.start..range.end] {
            "true" => Object::Boolean(true),
            "false" => Object::Boolean(false),
            _ => unreachable!(),
        }
    }

    fn eval_identifier(&self, range: &Range, env: &mut Env) -> Object {
        env.get(&self.input[range.start..range.end])
            .map(|a| a.clone())
            .unwrap_or(Object::Null)
    }

    fn eval_list(&self, values: &[ExpressionId], env: &mut Env) -> Object {
        let mut result = vec![];
        for expr in values {
            result.push(self.eval_expr(expr, env))
        }
        Object::List(result)
    }

    fn eval_map(&self, values: &[(ExpressionId, ExpressionId)], env: &mut Env) -> Object {
        let map = values
            .iter()
            .map(|(key, value)| (self.eval_expr(key, env), self.eval_expr(value, env)))
            .collect::<BTreeMap<Object, Object>>();

        Object::Map(map)
    }

    fn eval_unary(&self, op: UnaryOp, expr: &ExpressionId, env: &mut Env) -> Object {
        let value = self.eval_expr(expr, env);

        match op {
            OpNot => match value {
                Object::Boolean(b) => match b {
                    false => Object::Boolean(true),
                    true => Object::Boolean(false),
                },
                _ => unreachable!("Can't not non-boolean"),
            },
            OpNeg => match value {
                Object::Integer(i) => Object::Integer(-i),
                _ => unreachable!("Can't negate non-integer"),
            },
        }
    }

    fn eval_binary(
        &self,
        op: BinaryOp,
        left: &ExpressionId,
        right: &ExpressionId,
        env: &mut Env,
    ) -> Object {
        let left = self.eval_expr(left, env);
        let right = self.eval_expr(right, env);
        match (left, right) {
            (Object::Integer(l), Object::Integer(r)) => match op {
                BinaryOp::Plus => Object::Integer(l + r),
                BinaryOp::Minus => Object::Integer(l - r),
                BinaryOp::Times => Object::Integer(l * r),
                BinaryOp::Div => Object::Integer(l / r),
                BinaryOp::Greater => Object::Boolean(l > r),
                BinaryOp::Lesser => Object::Boolean(l < r),
                BinaryOp::Equals => Object::Boolean(l == r),
                BinaryOp::Differs => Object::Boolean(l != r),
            },

            (Object::Text(l), Object::Text(r)) => match op {
                BinaryOp::Plus => Object::Text(format!("{l}{r}")),
                BinaryOp::Minus => todo!(),
                BinaryOp::Times => todo!(),
                BinaryOp::Div => todo!(),
                BinaryOp::Greater => Object::Boolean(l > r),
                BinaryOp::Lesser => Object::Boolean(l < r),
                BinaryOp::Equals => Object::Boolean(l == r),
                BinaryOp::Differs => Object::Boolean(l != r),
            },

            (l, r) => match op {
                BinaryOp::Equals => Object::Boolean(l == r),
                BinaryOp::Differs => Object::Boolean(l != r),
                BinaryOp::Plus => unreachable!(),
                BinaryOp::Minus => unreachable!(),
                BinaryOp::Times => unreachable!(),
                BinaryOp::Div => unreachable!(),
                BinaryOp::Greater => unreachable!(),
                BinaryOp::Lesser => unreachable!(),
            },
        }
    }

    fn eval_conditional(
        &self,
        condition: &ExpressionId,
        positive: &[StatementId],
        negative: &[StatementId],
        env: &mut Env,
    ) -> Object {
        let ok = match self.eval_expr(condition, env) {
            Object::Boolean(b) => b,
            _ => unreachable!("No truthy around here baby"),
        };

        let statements = if ok { positive } else { negative };
        statements
            .iter()
            .map(|id| self.table.get_statement(id))
            .fold(Object::Null, |_, statement| {
                self.eval_statement(statement, env)
            })
    }

    fn eval_function(
        &self,
        range: &Range,
        parameters: &[ExpressionId],
        body: &[StatementId],
        env: &mut Env,
    ) -> Object {
        let name = &self.input[range.start..range.end];
        let func = Function {
            body: body.to_vec(),
            env: env.clone(),
            parameters: parameters
                .iter()
                .map(|id| self.table.get_expression(id).range)
                .map(|range| self.input[range.start..range.end].to_string())
                .collect(),
        };

        let result = Object::Function(func);
        env.insert(name.to_string(), result.clone());
        result
    }

    fn eval_call(
        &self,
        function: &ExpressionId,
        arguments: &[ExpressionId],
        outer_env: &mut Env,
    ) -> Object {
        let bindings = arguments
            .iter()
            .map(|el| self.eval_expr(el, outer_env))
            .collect::<Vec<_>>();

        let Function { parameters, body, env } = {
            let RawExpression { ref range, .. } = self.table.get_expression(function);
            let name = &self.input[range.start..range.end];
            let value = outer_env.get(name);
            match value {
                None => return Object::Null,
                Some(o) => match o {
                    Object::Function(f) => f.clone(),
                    _ => unreachable!(),
                },
            }
        };

        let mut env = env.clone();
        for (arg, param) in zip(parameters, bindings) {
            env.insert(arg, param);
        }

        let mut result = Object::Null;
        for id in body {
            let statement = self.table.get_statement(&id);
            result = self.eval_statement(&statement, &mut env)
        }

        result
    }
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeMap;

    use crate::flat_eval::{eval, Object};

    #[test]
    fn integers() {
        let input = r#"
        10
        "#;

        assert_eq!(eval(input), Object::Integer(10))
    }

    #[test]
    fn text() {
        let input = r#"
        "some_string"
        "#;

        assert_eq!(eval(input), Object::Text("some_string".to_string()))
    }

    #[test]
    fn booleans() {
        assert_eq!(eval("true"), Object::Boolean(true));
        assert_eq!(eval("false"), Object::Boolean(false));
    }

    #[test]
    fn parenthesized() {
        assert_eq!(eval("(true)"), Object::Boolean(true));
        assert_eq!(eval("(1)"), Object::Integer(1));
    }

    #[test]
    fn unary() {
        assert_eq!(eval("-1"), Object::Integer(-1));
        assert_eq!(eval("!false"), Object::Boolean(true));
    }

    #[test]
    fn binary() {
        assert_eq!(eval("2 + 1"), Object::Integer(3));
        assert_eq!(eval("2 - 1"), Object::Integer(1));
        assert_eq!(eval("2 * 1"), Object::Integer(2));
        assert_eq!(eval("2 / 1"), Object::Integer(2));
        assert_eq!(eval("2 > 1"), Object::Boolean(true));
        assert_eq!(eval("2 != 1"), Object::Boolean(true));
        assert_eq!(eval("2 == 1"), Object::Boolean(false));
        assert_eq!(eval("2 < 1"), Object::Boolean(false));
        assert_eq!(eval("\"a\" == \"a\""), Object::Boolean(true));
        assert_eq!(eval("\"a\" == \"b\""), Object::Boolean(false));
    }

    #[test]
    fn list() {
        assert_eq!(
            eval("[1, 2]"),
            Object::List(vec!(Object::Integer(1), Object::Integer(2)))
        );
    }

    #[test]
    fn map() {
        assert_eq!(
            eval(r#"{"a" : 1 , "b" : 2}"#),
            Object::Map(BTreeMap::from([
                (Object::Text("a".to_string()), Object::Integer(1)),
                (Object::Text("b".to_string()), Object::Integer(2)),
            ]))
        );
    }

    #[test]
    fn conditional() {
        assert_eq!(
            eval(r#"if ( 2 > 1 ) { 1 + 1 } else { 2 + 2 }"#),
            Object::Integer(2)
        );
        assert_eq!(
            eval(r#"if ( 2 < 1 ) { 1 + 1 } else { 2 + 2 }"#),
            Object::Integer(4)
        );
        assert_eq!(eval(r#"if ( true ) { } else { 2 + 2 }"#), Object::Null);
        assert_eq!(
            eval(r#"if ( true ) { 1 + 1 ; 2 + 2} else { 3 + 3 }"#),
            Object::Integer(4)
        );
    }

    #[test]
    fn let_statements() {
        let input = r#"
        let a = 1
        a
        "#;

        assert_eq!(eval(input), Object::Integer(1));
    }

    #[test]
    fn function_call() {
        let input = r#"
        let a = 2
        let double = fn( x ) {
            return a * x
        }
        double(4)
        "#;
        assert_eq!(eval(input), Object::Integer(8));
    }
}
