use std::cell::RefCell;
use std::collections::BTreeMap;
use std::iter::zip;
use std::ops::Deref;
use std::rc::Rc;

use crate::eval::Object::Error;
use crate::parser::StatementKind::EndStatement;
use crate::parser::{
    BinaryOp, Expression, ExpressionKind, Parser, StatementBlock, StatementKind, UnaryOp,
};

#[derive(Debug, Eq, PartialEq, Clone, PartialOrd, Ord)]
pub enum Object {
    Integer(i32),
    Boolean(Booleans),
    Str(String),
    Null,
    Return(Box<Object>),
    Error(String),
    Function(Function),
    Builtin(Builtin),
    Array(Vec<Box<Object>>),
    Hash(BTreeMap<Object, Object>),
}

#[derive(Debug, Eq, PartialEq, Clone, Ord, PartialOrd)]
pub enum Builtin {
    Len,
    First,
    Last,
    Rest,
    Push,
    Puts,
}

#[derive(Debug, Eq, PartialEq, Clone, Ord, PartialOrd)]
pub struct Function {
    parameters: Vec<String>,
    body: StatementBlock,
    env: Environment,
}

#[derive(Debug, Eq, PartialEq, Copy, Clone, Ord, PartialOrd)]
pub enum Booleans {
    True,
    False,
}

pub struct VM {
    env: Environment,
}

#[derive(Debug, Eq, PartialEq, Clone, Ord, PartialOrd)]
struct EnvironmentInner {
    bindings: BTreeMap<String, Object>,
    parent: Option<Environment>,
}

#[derive(Debug, Eq, PartialEq, Clone, Ord, PartialOrd)]
pub struct Environment {
    env: Rc<RefCell<EnvironmentInner>>,
}

impl Environment {
    pub fn new() -> Environment {
        let mut map = BTreeMap::default();
        map.insert("len".to_owned(), Object::Builtin(Builtin::Len));
        map.insert("first".to_owned(), Object::Builtin(Builtin::First));
        map.insert("last".to_owned(), Object::Builtin(Builtin::Last));
        map.insert("rest".to_owned(), Object::Builtin(Builtin::Rest));
        map.insert("push".to_owned(), Object::Builtin(Builtin::Push));
        map.insert("puts".to_owned(), Object::Builtin(Builtin::Puts));
        return Environment {
            env: Rc::new(RefCell::new(EnvironmentInner {
                bindings: map,
                parent: None,
            })),
        };
    }
    pub fn from(map: BTreeMap<String, Object>) -> Environment {
        return Environment {
            env: Rc::new(RefCell::new(EnvironmentInner {
                bindings: map,
                parent: None,
            })),
        };
    }
    pub fn extend(env: &Environment) -> Environment {
        return Environment {
            env: Rc::new(RefCell::new(EnvironmentInner {
                bindings: BTreeMap::default(),
                parent: Some(Environment { env: env.env.clone() }),
            })),
        };
    }
    pub fn add(&self, name: String, expr: Object) {
        self.env.borrow_mut().bindings.insert(name, expr);
    }
    pub fn get(&self, name: &str) -> Option<Object> {
        let x = self.env.borrow();
        return match (x.bindings.get(name), &x.parent) {
            (Some(v), _) => Some(v.clone()),
            (None, Some(p)) => p.get(name),
            (_, _) => None,
        };
    }
}

pub fn evaluate(s: &str) -> Object {
    let vm = VM::new();
    return vm.eval_source(s);
}

impl VM {
    fn new() -> VM {
        return VM { env: Environment::new() };
    }
    fn eval_source(&self, str: &str) -> Object {
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

    fn eval_program(&self, v: Vec<StatementKind>) -> Object {
        let mut result = Object::Null;

        for e in v {
            result = match self.eval_statement(&e, &self.env) {
                Object::Return(ret) => return *ret,
                error @ Object::Error { .. } => return error,
                other => other,
            }
        }
        result
    }

    fn eval_statement(&self, stmt: &StatementKind, env: &Environment) -> Object {
        match stmt {
            StatementKind::ExprStmt { expr } => self.eval_expression(&expr.kind, env),
            StatementKind::ReturnStmt { expr } => {
                Object::Return(Box::from(self.eval_expression(&expr.kind, env)))
            }
            StatementKind::LetStmt { name, expr } => match self.eval_expression(&expr.kind, env) {
                e @ Error(_) => e,
                Object::Return(_) => Error("let return is not valid".to_owned()),
                obj => {
                    env.add(name.to_owned(), obj.clone());
                    obj
                }
            },
            StatementKind::IllegalStatement { expr } => {
                Error(format!("Illegal Statement: {:?}", expr))
            }
            EndStatement => todo!(),
        }
    }

    fn eval_expression(&self, expr: &ExpressionKind, env: &Environment) -> Object {
        match expr {
            ExpressionKind::LiteralInteger { value } => Object::Integer(*value),
            ExpressionKind::LiteralString { value } => Object::Str(value.clone()),
            ExpressionKind::LiteralBoolean { value } => as_boolean(*value),
            ExpressionKind::LiteralArray { values } => {
                let v: Vec<Box<Object>> = values
                    .iter()
                    .map(|v| self.eval_expression(&v.kind, env).into())
                    .collect();
                Object::Array(v)
            }
            ExpressionKind::Unary { op, expr } => match (op, &expr.kind) {
                (UnaryOp::OpNot, k) => match self.eval_expression(k, env) {
                    Object::Boolean(Booleans::True) => Object::Boolean(Booleans::False),
                    Object::Boolean(Booleans::False) => Object::Boolean(Booleans::True),
                    Object::Null => Object::Boolean(Booleans::False),
                    _ => Object::Boolean(Booleans::False),
                },

                (UnaryOp::OpNeg, k) => match self.eval_expression(k, env) {
                    Object::Integer(value) => Object::Integer(-value),
                    obj => Error(format!("Unknown operator: -{:?}", obj)),
                },
            },
            ExpressionKind::LiteralHash { values } => {
                let map = values
                    .iter()
                    .map(|(a, b)| {
                        (
                            self.eval_expression(&a.kind, env),
                            self.eval_expression(&b.kind, env),
                        )
                    })
                    .collect::<BTreeMap<Object, Object>>();
                Object::Hash(map)
            }
            ExpressionKind::LiteralFunction { parameters, body } => Object::Function(Function {
                parameters: parameters.clone(),
                body: body.clone(),
                env: Environment::extend(env),
            }),
            ExpressionKind::IndexExpression { left, idx } => {
                let index = self.eval_expression(&idx.kind, env);
                let lobj = self.eval_expression(&left.kind, env);

                match (lobj, index) {
                    (Object::Array(values), Object::Integer(i)) => match values.get(i as usize) {
                        Some(v) => *v.clone(),
                        None => Object::Error("indexing error".into()),
                    },

                    (Object::Str(values), Object::Integer(value)) => {
                        Object::Str(values.as_bytes()[value as usize].to_string())
                    }
                    (Object::Hash(values), o) => values
                        .get(&o)
                        .map(|o| o.clone())
                        .unwrap_or_else(|| Object::Null),

                    (_, _) => Error("Not sure what this indexing is".into()),
                }
            }
            ExpressionKind::Identifier { name } => match env.get(name) {
                None => Error(format!("Unknown identifier {:?}", name)),
                Some(expr) => expr.clone(),
            },

            ExpressionKind::Binary { op, left, right } => {
                match (
                    self.eval_expression(&left.kind, env),
                    self.eval_expression(&right.kind, env),
                ) {
                    (Object::Integer(l), Object::Integer(r)) => self.eval_binary_int(op, l, r),
                    (Object::Boolean(l), Object::Boolean(r)) => self.eval_binary_bool(op, l, r),
                    (Object::Str(l), Object::Str(r)) => self.eval_binary_str(op, &l, &r),
                    (l, r) => Error(format!("Unknown operator: {:?} {:?} {:?}", l, op, r)),
                }
            }
            e @ ExpressionKind::Conditional { condition, positive, negative } => {
                self.eval_conditional(condition, positive, negative, env)
            }

            ExpressionKind::Call { function, arguments } => {
                let mut args: Vec<Object> = vec![];
                for a in arguments {
                    match self.eval_expression(&a.kind, env) {
                        e @ (Object::Return(_) | Error(_)) => {
                            return e;
                        }

                        o => args.push(o),
                    }
                }

                match self.eval_expression(&function.as_ref().kind, env) {
                    Object::Function(f) => {
                        let fenv = Environment::extend(&f.env);
                        for (k, v) in zip(f.parameters, args) {
                            fenv.add(k, v);
                        }
                        match self.eval_block(&f.body, &fenv) {
                            Object::Return(o) => *o,
                            e => e,
                        }
                    }
                    Object::Builtin(b) => {
                        return match b {
                            Builtin::Len => match args.as_slice() {
                                [] => Error("Expected args".to_owned()),
                                [Object::Array(s)] => Object::Integer(s.len() as i32),
                                [Object::Str(s)] => Object::Integer(s.len() as i32),
                                a => Error(format!("Expected single Str arg, got {:?}", a)),
                            },
                            Builtin::First => match args.as_slice() {
                                [] => Error("Expected args".to_owned()),
                                [Object::Array(s)] => s
                                    .first()
                                    .map(|o| o.as_ref().clone())
                                    .unwrap_or_else(|| Object::Null),
                                [Object::Str(s)] => {
                                    Object::Str(s.chars().nth(0).unwrap().to_string())
                                }
                                a => Error(format!("Expected single Str arg, got {:?}", a)),
                            },
                            Builtin::Last => match args.as_slice() {
                                [] => Error("Expected args".to_owned()),
                                [Object::Array(s)] => s
                                    .last()
                                    .map(|o| o.as_ref().clone())
                                    .unwrap_or_else(|| Object::Null),
                                [Object::Str(s)] => {
                                    Object::Str(s.chars().rev().nth(0).unwrap().to_string())
                                }
                                a => Error(format!("Expected single Str arg, got {:?}", a)),
                            },
                            Builtin::Rest => match args.as_slice() {
                                [] => Error("Expected args".to_owned()),
                                [Object::Array(s)] => {
                                    Object::Array(s.iter().skip(1).map(|o| o.clone()).collect())
                                }
                                a => Error(format!("Expected single Str arg, got {:?}", a)),
                            },
                            Builtin::Push => match args.as_slice() {
                                [] => Error("Expected args".to_owned()),
                                [Object::Array(s), o] => {
                                    let mut s2 = s.clone();
                                    s2.push(Box::from(o.clone()));
                                    Object::Array(s2)
                                }
                                a => Error(format!("Expected single Str arg, got {:?}", a)),
                            },
                            Builtin::Puts => {
                                for x in args {
                                    println!("{:?}", x)
                                }
                                Object::Null
                            }
                        }
                    }
                    o @ Error { .. } => return o,
                    o => return Error(format!("expected function, got {:?}", o)),
                }
            }
            ExpressionKind::IllegalExpression { .. } => todo!(),
            ww => todo!(),
        }
    }

    fn eval_conditional(
        &self,
        condition: &Box<Expression>,
        positive: &StatementBlock,
        negative: &Option<StatementBlock>,
        env: &Environment,
    ) -> Object {
        match self.eval_expression(&condition.kind, env) {
            Object::Boolean(b) => match b {
                Booleans::True => self.eval_block(positive, env),
                Booleans::False => match negative {
                    None => Object::Null,
                    Some(block) => self.eval_block(block, env),
                },
            },
            Object::Integer(i) => self.eval_block(positive, env),
            Object::Error(m) => Error(m),
            _ => Object::Null,
        }
    }

    fn eval_block(&self, block: &StatementBlock, env: &Environment) -> Object {
        let mut result = Object::Null;
        for x in &block.statements {
            result = self.eval_statement(&x.kind, env);
            if matches!(result, Object::Return(_)) {
                return result;
            }
            if matches!(result, Object::Error(_)) {
                return result;
            }
        }
        return result;
    }

    fn eval_binary_str(&self, op: &BinaryOp, l: &str, r: &str) -> Object {
        return match op {
            BinaryOp::OpPlus => Object::Str(format!("{l}{r}")),
            op => Error(format!("Unknown operator: {:?} {:?} {:?}", l, op, r)),
        };
    }

    fn eval_binary_bool(&self, op: &BinaryOp, l: Booleans, r: Booleans) -> Object {
        match op {
            BinaryOp::OpEquals => as_boolean(l == r),
            BinaryOp::OpDiffers => as_boolean(l != r),
            op => Error(format!("Unknown operator: {:?} {:?} {:?}", l, op, r)),
        }
    }

    fn eval_binary_int(&self, op: &BinaryOp, l: i32, r: i32) -> Object {
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
    use crate::eval::Booleans::{False, True};
    use crate::eval::Object::{Boolean, Integer, Str};
    use crate::lexer::Span;
    use crate::parser::ExpressionKind::Binary;
    use crate::parser::Statement;
    use crate::parser::StatementKind::ExprStmt;

    use super::*;

    fn evaluate(s: &str) -> Object {
        let vm = VM::new();
        return vm.eval_source(s);
    }
    #[test]
    fn literal_int() {
        let input = "10";
        assert_eq!(evaluate(input), Object::Integer(10))
    }

    #[test]
    fn literal_bool() {
        let input = "true";
        assert_eq!(evaluate(input), Object::Boolean(Booleans::True));
    }

    #[test]
    fn unaries() {
        assert_eq!(evaluate("!5"), Object::Boolean(Booleans::False));
        assert_eq!(evaluate("!true"), Object::Boolean(Booleans::False));
        assert_eq!(evaluate("!!false"), Object::Boolean(Booleans::False));

        assert_eq!(evaluate("!!5"), Object::Boolean(Booleans::True));
        assert_eq!(evaluate("!false"), Object::Boolean(Booleans::True));
        assert_eq!(evaluate("!!true"), Object::Boolean(Booleans::True));

        assert_eq!(evaluate("-5"), Object::Integer(-5));
        assert_eq!(evaluate("!-5"), Object::Boolean(Booleans::False));
        assert_eq!(evaluate("!!-5"), Object::Boolean(Booleans::True));
        assert_eq!(
            evaluate("-true"),
            Error("Unknown operator: -Boolean(True)".to_owned())
        );
    }

    #[test]
    fn int_binaries() {
        assert_eq!(evaluate("5 + 5"), Object::Integer(10));
        assert_eq!(evaluate("5 - 5"), Object::Integer(0));
        assert_eq!(evaluate("5 * 5"), Object::Integer(25));
        assert_eq!(evaluate("5 / 5"), Object::Integer(1));
        assert_eq!(evaluate("5 / 0"), Error("Cannot divide by 0".to_owned()));

        assert_eq!(evaluate("5 > 5"), Object::Boolean(Booleans::False));
        assert_eq!(evaluate("5 < 5"), Object::Boolean(Booleans::False));
        assert_eq!(evaluate("5 != 5"), Object::Boolean(Booleans::False));
        assert_eq!(evaluate("5 == 5"), Object::Boolean(Booleans::True));

        assert_eq!(
            evaluate("(5 + 10 * 2 + 15 / 3 ) * 2 + -10"),
            Object::Integer(50)
        );
    }

    #[test]
    fn bool_binaries() {
        assert_eq!(evaluate("true == true"), Object::Boolean(Booleans::True));
        assert_eq!(evaluate("true == false"), Object::Boolean(Booleans::False));

        assert_eq!(evaluate("true != true"), Object::Boolean(Booleans::False));
        assert_eq!(evaluate("true != false"), Object::Boolean(Booleans::True));

        assert_eq!(evaluate("(1 < 2) == true"), Object::Boolean(Booleans::True));
        assert_eq!(
            evaluate("(1 < 2) == false"),
            Object::Boolean(Booleans::False)
        );

        assert_eq!(
            evaluate("(1 > 2) == true"),
            Object::Boolean(Booleans::False)
        );
        assert_eq!(
            evaluate("(1 > 2) == false"),
            Object::Boolean(Booleans::True)
        );
    }

    #[test]
    fn conditionals() {
        assert_eq!(evaluate("if (true) { 10 }"), Object::Integer(10));
        assert_eq!(evaluate("if (false) { 10 }"), Object::Null);
        assert_eq!(evaluate("if (1) { 10 }"), Object::Integer(10));
        assert_eq!(evaluate("if (1 < 2 ) { 10 }"), Object::Integer(10));
        assert_eq!(evaluate("if (1 > 2 ) { 10 }"), Object::Null);

        assert_eq!(
            evaluate("if (1 > 2 ) { 10 } else { 20 }"),
            Object::Integer(20)
        );
        assert_eq!(
            evaluate("if (1 < 2 ) { 10 } else { 20 }"),
            Object::Integer(10)
        );
    }

    #[test]
    fn returns() {
        assert_eq!(evaluate("9; return 2 * 5; 9;"), Object::Integer(10));

        let source = "
        if (10 > 1) {
            if (10 > 1) {
                return 10;
            }

            return 1;
        }";
        assert_eq!(evaluate(source), Object::Integer(10));
    }

    #[test]
    fn errors() {
        assert_eq!(
            evaluate("5 + true"),
            Object::Error("Unknown operator: Integer(5) OpPlus Boolean(True)".to_owned())
        );
        assert_eq!(
            evaluate("5 + true; 5"),
            Object::Error("Unknown operator: Integer(5) OpPlus Boolean(True)".to_owned())
        );
        assert_eq!(
            evaluate("-true"),
            Object::Error("Unknown operator: -Boolean(True)".to_owned())
        );

        let source = "
        if (10 > 1) {
            if (10 > 1) {
                return true * false;
            }
            return 1;
        }";
        assert_eq!(
            evaluate(source),
            Object::Error("Unknown operator: True OpTimes False".to_owned())
        );
    }

    #[test]
    fn bindings() {
        assert_eq!(evaluate("let a = 5; a;"), Object::Integer(5));
        assert_eq!(evaluate("let a = 5 * 5; a;"), Object::Integer(25));
        assert_eq!(evaluate("let a = 5; let b = a; b"), Object::Integer(5));
        assert_eq!(
            evaluate("let a = 5; let b = a; let c = a + b + 5; c;"),
            Object::Integer(15)
        );
        assert_eq!(
            evaluate("foobar"),
            Object::Error("Unknown identifier \"foobar\"".to_owned())
        );
    }

    #[test]
    fn functions() {
        assert_eq!(
            evaluate("fn(x) { x + 2; };"),
            Object::Function(Function {
                parameters: vec!("x".into()),
                env: Environment::extend(&Environment::new()),
                body: StatementBlock {
                    span: Span { start: 6, end: 16 },
                    statements: vec!(Statement {
                        span: Span { start: 8, end: 14 },
                        kind: ExprStmt {
                            expr: Expression {
                                span: Span { start: 8, end: 13 },
                                kind: Binary {
                                    op: BinaryOp::OpPlus,
                                    left: Expression {
                                        span: Span { start: 8, end: 9 },
                                        kind: ExpressionKind::Identifier { name: "x".into() },
                                    }
                                    .into(),
                                    right: Expression {
                                        span: Span { start: 12, end: 13 },
                                        kind: ExpressionKind::LiteralInteger { value: 2 },
                                    }
                                    .into(),
                                },
                            },
                        },
                    }
                    .into(),),
                },
            })
        )
    }

    #[test]
    fn calls() {
        assert_eq!(
            evaluate("let identity = fn(x) { x; }; identity(5);"),
            Object::Integer(5)
        );

        assert_eq!(
            evaluate("let double = fn(x) { x * 2; }; double(5);"),
            Object::Integer(10)
        );

        assert_eq!(
            evaluate("let add = fn(x, y) { x + y; }; add(5, 5);"),
            Object::Integer(10)
        );

        assert_eq!(
            evaluate("let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));"),
            Object::Integer(20)
        );

        assert_eq!(evaluate("fn(x) { x; }(5)"), Object::Integer(5));

        assert_eq!(
            evaluate("let factorial = fn(x) { if ( x == 1 ) { return 1 } else { return x * factorial( x - 1 )} }; factorial( 5 );"),
                Object::Integer(120)
        );

        assert_eq!(
            evaluate("let myFun = fn( a ) { return fn( b ) { b * a } }; myFun(2)(3) "),
            Object::Integer(6)
        )
    }

    #[test]
    fn strings() {
        assert_eq!(
            evaluate(
                "
                let hello = \"Hello \";
                let world = \"World\";
                hello + world
                 "
            ),
            Object::Str("Hello World".into())
        );
    }

    #[test]
    fn builtins() {
        assert_eq!(evaluate(r#"len("")"#), Object::Integer(0));
        assert_eq!(evaluate(r#"len("four")"#), Object::Integer(4));
        assert_eq!(evaluate(r#"len("hello world")"#), Object::Integer(11));
        assert_eq!(
            evaluate(r#"len(1)"#),
            Object::Error("Expected single Str arg, got [Integer(1)]".to_owned())
        );

        assert_eq!(
            evaluate(r#"len("one" , "two")"#),
            Object::Error("Expected single Str arg, got [Str(\"one\"), Str(\"two\")]".to_owned())
        );

        assert_eq!(evaluate(r#"len([])"#), Object::Integer(0));
        assert_eq!(evaluate(r#"len([1])"#), Object::Integer(1));
        assert_eq!(evaluate(r#"len([1, "2", fn(){3}])"#), Object::Integer(3));
        assert_eq!(evaluate(r#"first([])"#), Object::Null);
        assert_eq!(evaluate(r#"last([])"#), Object::Null);
        assert_eq!(evaluate(r#"first([1])"#), Object::Integer(1));
        assert_eq!(evaluate(r#"last([fn(){3}, "2", 1])"#), Object::Integer(1));
    }

    #[test]
    fn arrays() {
        assert_eq!(
            evaluate(r#" [1, 2 * 2, 3 + 3] "#),
            Object::Array(vec!(
                Object::Integer(1).into(),
                Object::Integer(4).into(),
                Object::Integer(6).into()
            ))
        );

        assert_eq!(
            evaluate(r#" [1, 2 * 2, 3 + 3][fn() { 2 }()]"#),
            Object::Integer(6).into()
        );

        assert_eq!(
            evaluate(r#" [1, 2 * 2, 3 + 3][fn(x) { x }(0)]"#),
            Object::Integer(1).into()
        );
    }

    #[test]
    fn map_reduce() {
        let map_input = r#"
        let map = fn(arr, f) {
            let iter = fn(arr, accumulated) {
                if (len(arr) == 0) {
                    accumulated
                } else {
                    iter(rest(arr), push(accumulated, f(first(arr))));
                }
            };
            iter(arr, []);
        };
        let a = [1, 2, 3, 4];
        let double = fn(x) { x * 2 };
        map(a, double);
        "#;

        assert_eq!(
            evaluate(map_input),
            Object::Array(vec!(
                Object::Integer(2).into(),
                Object::Integer(4).into(),
                Object::Integer(6).into(),
                Object::Integer(8).into()
            ))
        );

        let reduce_input = r#"
        let reduce = fn(arr, initial, f) {
            let iter = fn(arr, result) {
                if (len(arr) == 0) {
                    result
                } else {
                    iter(rest(arr), f(result, first(arr)));
                }
            };
            iter(arr, initial);
        };

        let sum = fn(arr) {
            reduce(arr, 0, fn(initial, el) { initial + el });
        };
        sum([1,2,3,4,5])
        "#;

        assert_eq!(evaluate(reduce_input), Object::Integer(15));
    }

    #[test]
    fn hashes() {
        let hash_input = r#"
        let two = "two";
        {
            "one"       : 10 - 9,
            two         : 1 + 1,
            "thr" + "ee": 6 / 2,
            4           : 4,
            true        : 5,
            false       : 6
        }
        "#;

        assert_eq!(
            evaluate(hash_input),
            Object::Hash(BTreeMap::from([
                (Integer(4), Integer(4)),
                (Boolean(True), Integer(5)),
                (Boolean(False), Integer(6)),
                (Str("one".into()), Integer(1)),
                (Str("three".into()), Integer(3)),
                (Str("two".into()), Integer(2)),
            ]))
        );

        let index_input = r#"
        {
            "one"       : 10 - 9,
            "two"       : 1 + 1,
            "thr" + "ee": 6 / 2,
            4           : 4,
            true        : 5,
            false       : 6
        }[true]
        "#;

        assert_eq!(evaluate(index_input), Integer(5));
    }
}
