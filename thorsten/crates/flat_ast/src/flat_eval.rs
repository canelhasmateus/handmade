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
pub enum Object {
    Null,
    Integer(i64),
    Text(String),
    Boolean(bool),
    List(Vec<Object>),
    Map(BTreeMap<Object, Object>),
    Function(Function),
}

#[derive(Debug, PartialEq, PartialOrd, Eq, Ord, Clone)]
pub enum Return {
    Value(Object),
    Return(Object),
    Error(String),
}

#[derive(Debug, PartialEq, PartialOrd, Eq, Ord, Clone)]
pub struct Function {
    parameters: Vec<String>,
    body: Vec<StatementId>,
    env: Env,
}

struct VM<'a> {
    table: ExprTable,
    input: &'a str,
}

pub fn eval(input: &str) -> Object {
    let mut table = ExprTable {
        statements: Vec::new(),
        expressions: Vec::new(),
    };
    let statements = raw_statements(input, &mut table);

    let vm = VM { table, input };
    let mut env = BTreeMap::new();
    let mut result = Return::Value(Object::Null);
    for statement in statements {
        result = vm.eval_statement(&statement, &mut env);
    }
    match result {
        Return::Value(v) => v,
        Return::Return(r) => r,
        Return::Error(e) => Object::Text(e), // lol
    }
}

impl VM<'_> {
    fn eval_statement(&self, statement: &RawStatement, env: &mut Env) -> Return {
        match statement.kind {
            RawStatementKind::ExprStmt { ref expr } => self.eval_expr(expr, env),
            RawStatementKind::ReturnStmt { ref expr } => match self.eval_expr(expr, env) {
                Return::Value(v) => Return::Return(v),
                Return::Return(_) => unreachable!("return in return position"),
                e @ Return::Error(_) => e,
            },
            RawStatementKind::LetStmt { ref name, ref expr } => {
                let RawExpression { ref range, .. } = self.table.get_expression(name);
                let name = &self.input[range.start..range.end];
                match self.eval_expr(expr, env) {
                    Return::Return(_) => unreachable!("Return in let statement position"),
                    e @ Return::Error(_) => e,
                    Return::Value(v) => {
                        env.insert(name.to_string(), v);
                        Return::Value(Object::Null)
                    }
                }
            }

            RawStatementKind::EndStatement => todo!(),
            RawStatementKind::IllegalStatement => todo!(),
        }
    }

    fn eval_expr(&self, expr: &ExpressionId, env: &mut Env) -> Return {
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
            RawExpressionKind::IndexExpression { ref left, ref idx } => {
                self.eval_index(left, idx, env)
            }
            RawExpressionKind::IllegalExpression => todo!(),
        }
    }

    fn eval_integer(&self, range: &Range) -> Return {
        let content = &self.input[range.start..range.end];
        content
            .parse::<i64>()
            .map(|i| Return::Value(Object::Integer(i)))
            .unwrap_or_else(|_| Return::Error(format!("Cannot parse {} as integer", content)))
    }

    fn eval_text(&self, range: &Range) -> Return {
        let unquoted = (range.start + 1)..(range.end - 1);
        let content = self.input[unquoted].to_string();
        Return::Value(Object::Text(content))
    }

    fn eval_bool(&self, range: &Range) -> Return {
        let content = &self.input[range.start..range.end];
        content
            .parse::<bool>()
            .map(|b| Return::Value(Object::Boolean(b)))
            .unwrap_or_else(|_| Return::Error(format!("Cannot parse {} as boolean", content)))
    }

    fn eval_identifier(&self, range: &Range, env: &mut Env) -> Return {
        let key = &self.input[range.start..range.end];
        let value = env.get(key).cloned().unwrap_or(Object::Null);
        Return::Value(value)
    }

    fn eval_list(&self, values: &[ExpressionId], env: &mut Env) -> Return {
        let mut result = vec![];
        for expr in values {
            match self.eval_expr(expr, env) {
                Return::Value(b) => result.push(b),
                Return::Return(_) => unreachable!("Return expr inside a list?"),
                e @ Return::Error(_) => return e,
            }
        }
        Return::Value(Object::List(result))
    }

    fn eval_map(&self, values: &[(ExpressionId, ExpressionId)], env: &mut Env) -> Return {
        let mut map = BTreeMap::<Object, Object>::new();
        for (key, value) in values {
            let key = match self.eval_expr(key, env) {
                Return::Value(v) => v,
                Return::Return(_) => unreachable!("Return in map key position?"),
                e @ Return::Error(_) => return e,
            };

            let value = match self.eval_expr(value, env) {
                Return::Value(v) => v,
                Return::Return(_) => unreachable!("Return in map value position?"),
                e @ Return::Error(_) => return e,
            };

            map.insert(key, value);
        }

        Return::Value(Object::Map(map))
    }

    fn eval_unary(&self, op: UnaryOp, expr: &ExpressionId, env: &mut Env) -> Return {
        let value = match self.eval_expr(expr, env) {
            Return::Value(v) => v,
            e @ Return::Error(_) => return e,
            Return::Return(_) => unreachable!("Return in unary position"),
        };

        let result = match op {
            OpNot => match value {
                Object::Boolean(b) => Object::Boolean(!b),
                _ => unreachable!("Can't not non-boolean"),
            },
            OpNeg => match value {
                Object::Integer(i) => Object::Integer(-i),
                _ => unreachable!("Can't negate non-integer"),
            },
        };

        Return::Value(result)
    }

    fn eval_binary(
        &self,
        op: BinaryOp,
        left: &ExpressionId,
        right: &ExpressionId,
        env: &mut Env,
    ) -> Return {
        let left = match self.eval_expr(left, env) {
            Return::Value(v) => v,
            Return::Return(_) => unreachable!("Return in binary position"),
            e @ Return::Error(_) => return e,
        };

        let right = match self.eval_expr(right, env) {
            Return::Value(v) => v,
            Return::Return(_) => unreachable!("Return in binary position"),
            e @ Return::Error(_) => return e,
        };

        let result = match (left, right) {
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
                BinaryOp::Greater => Object::Boolean(l > r),
                BinaryOp::Lesser => Object::Boolean(l < r),
                BinaryOp::Equals => Object::Boolean(l == r),
                BinaryOp::Differs => Object::Boolean(l != r),
                BinaryOp::Minus => return Return::Error("Cant subtract strings".to_owned()),
                BinaryOp::Times => return Return::Error("Cant multiply strings".to_owned()),
                BinaryOp::Div => return Return::Error("Cant divide strings".to_owned()),
            },

            (l, r) => match op {
                BinaryOp::Equals => Object::Boolean(l == r),
                BinaryOp::Differs => Object::Boolean(l != r),
                BinaryOp::Plus => return Return::Error("Cant add these things".to_owned()),
                BinaryOp::Minus => return Return::Error("Cant subtract these things".to_owned()),
                BinaryOp::Times => return Return::Error("Cant multiply these things".to_owned()),
                BinaryOp::Div => return Return::Error("Cant divide these things".to_owned()),
                BinaryOp::Greater => return Return::Error("Cant compare these things".to_owned()),
                BinaryOp::Lesser => return Return::Error("Cant compare these things".to_owned()),
            },
        };

        Return::Value(result)
    }

    fn eval_conditional(
        &self,
        condition: &ExpressionId,
        positive: &[StatementId],
        negative: &[StatementId],
        env: &mut Env,
    ) -> Return {
        let ok = match self.eval_expr(condition, env) {
            Return::Return(_) => unreachable!("Return in if condition"),
            e @ Return::Error(_) => return e,
            Return::Value(v) => match v {
                Object::Boolean(b) => b,
                _ => unreachable!("No truthy around here baby"),
            },
        };

        let statements = if ok { positive } else { negative };
        let mut result = Return::Value(Object::Null);
        for statement in statements {
            let statement = self.table.get_statement(statement);
            match self.eval_statement(statement, env) {
                v @ Return::Value(_) => result = v,
                r @ Return::Return(_) => return r,
                e @ Return::Error(_) => return e,
            };
        }
        result
    }

    fn eval_function(
        &self,
        range: &Range,
        parameters: &[ExpressionId],
        body: &[StatementId],
        env: &mut Env,
    ) -> Return {
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
        Return::Value(result)
    }

    fn eval_call(
        &self,
        function: &ExpressionId,
        arguments: &[ExpressionId],
        outer_env: &mut Env,
    ) -> Return {
        let mut bindings = vec![];
        for arg in arguments {
            match self.eval_expr(arg, outer_env) {
                Return::Value(v) => bindings.push(v),
                Return::Return(_) => unreachable!("Return in call position"),
                e @ Return::Error(_) => return e,
            }
        }

        let Function { parameters, body, env } = {
            let RawExpression { ref range, .. } = self.table.get_expression(function);
            let name = &self.input[range.start..range.end];
            let value = outer_env.get(name);
            match value {
                None => return Return::Error(format!("Function not found: {}", name)),
                Some(o) => match o {
                    Object::Function(f) => f.clone(),
                    _ => unreachable!(),
                },
            }
        };

        let mut env = {
            let mut e = outer_env.clone();
            e.extend(env);
            for (arg, param) in zip(parameters, bindings) {
                e.insert(arg, param);
            }
            e
        };

        let mut result = Object::Null;
        for id in body {
            let statement = self.table.get_statement(&id);
            match self.eval_statement(statement, &mut env) {
                Return::Value(v) => result = v,
                Return::Return(r) => return Return::Value(r),
                e @ Return::Error(_) => return e,
            }
        }

        Return::Value(result)
    }

    fn eval_index(&self, left: &ExpressionId, idx: &ExpressionId, env: &mut Env) -> Return {
        let left = match self.eval_expr(left, env) {
            Return::Value(v) => v,
            Return::Return(_) => unreachable!("Return in indexing position"),
            e @ Return::Error(_) => return e,
        };

        let idx = match self.eval_expr(idx, env) {
            Return::Value(v) => v,
            Return::Return(_) => unreachable!("Return in indexand position"),
            e @ Return::Error(_) => return e,
        };

        let result = match (left, idx) {
            (Object::List(values), Object::Integer(i)) => match values.get(i as usize) {
                Some(v) => v.clone(),
                None => unreachable!(),
            },

            (Object::Text(l), Object::Integer(r)) => {
                let val = l.as_bytes()[r as usize] as char;
                Object::Text(String::from(val))
            }

            (Object::Map(values), o) => values.get(&o).cloned().unwrap_or(Object::Null),

            _ => unreachable!(),
        };

        Return::Value(result)
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
        assert_eq!(eval("\"a\" + \"b\""), Object::Text("ab".to_string()));
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

    #[test]
    fn index() {
        let input = r#"
        let map = { true : "a"}
        let text= "abacate"
        let list = [true, false, "c"] 
        map[true] + text[1] + list[2]
        "#;
        assert_eq!(eval(input), Object::Text("abc".to_string()));
    }

    #[test]
    fn early_returns() {
        let input = r#"
        let conditional = fn() {
            if ( 1 < 2 ) {  
                return true
            }
            return false
        }
        conditional()
        "#;
        assert_eq!(eval(input), Object::Boolean(true));
    }

    #[test]
    fn fibonacci() {
        let input = r#"
        let fibonacci = fn(x) {
            if (x < 1) {  
                return 0
            }
            if (x<2) {
            return 1 }

            return fibonacci(x-1) + fibonacci(x-2)
        }
        fibonacci(10)
        "#;
        assert_eq!(eval(input), Object::Integer(55));
    }
}
