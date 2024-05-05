use core::fmt;

use crate::{
    bytecode::{
        self, add, bbool, bint, cte, div, eq, gt, lt, mul, neg, neq, not, pop, sub, ByteObj,
        Bytecode, ConstantId, Operation,
    },
    parser::{
        ExprTable, ExpressionId, RawExpression, RawExpressionKind, RawStatement, RawStatementKind,
    },
};

struct Instructions {
    instructions: Vec<Operation>,
}

struct Compiler {}

struct CompUnit<'a> {
    source: &'a str,
    table: ExprTable,
    statements: Vec<RawStatement>,
}

impl Compiler {
    pub fn new() -> Compiler {
        Compiler {}
    }

    pub fn compile(&self, unit: &CompUnit) -> Bytecode {
        let mut res = Bytecode { constants: vec![], instructions: vec![] };
        for statement in &unit.statements {
            self.compile_statement(unit, statement, &mut res)
        }
        res
    }

    fn compile_statement(&self, unit: &CompUnit, statement: &RawStatement, res: &mut Bytecode) {
        match statement.kind {
            RawStatementKind::LetStmt { name, expr } => todo!(),
            RawStatementKind::ReturnStmt { expr } => todo!(),
            RawStatementKind::ExprStmt { expr } => {
                self.compile_expr(unit, &expr, res);
                res.emit(pop())
            }
            RawStatementKind::IllegalStatement => todo!(),
            RawStatementKind::EndStatement => todo!(),
        }
    }

    fn compile_expr(&self, unit: &CompUnit, expr: &ExpressionId, res: &mut Bytecode) {
        let RawExpression { range, kind } = unit.table.get_expression(&expr);
        match kind {
            RawExpressionKind::LiteralInteger => {
                let value = (&unit.source[range]).parse::<i64>().unwrap();
                let id = res.add(bint(value));
                res.emit(Operation::Cte(id));
            }
            RawExpressionKind::Binary { op, left, right } => {
                self.compile_expr(unit, left, res);
                self.compile_expr(unit, right, res);
                match op {
                    crate::parser::BinaryOp::Plus => res.emit(add()),
                    crate::parser::BinaryOp::Minus => res.emit(sub()),
                    crate::parser::BinaryOp::Times => res.emit(mul()),
                    crate::parser::BinaryOp::Div => res.emit(div()),
                    crate::parser::BinaryOp::Greater => res.emit(gt()),
                    crate::parser::BinaryOp::Lesser => res.emit(lt()),
                    crate::parser::BinaryOp::Equals => res.emit(eq()),
                    crate::parser::BinaryOp::Differs => res.emit(neq()),
                }
            }
            RawExpressionKind::LiteralString => todo!(),
            RawExpressionKind::LiteralBoolean => {
                let value = (&unit.source[range]).parse::<bool>().unwrap();
                let id = res.add(bbool(value));
                res.emit(Operation::Cte(id));
            }
            RawExpressionKind::Parenthesized { expr } => self.compile_expr(unit, expr, res),
            RawExpressionKind::LiteralFunction { parameters, body } => todo!(),
            RawExpressionKind::LiteralArray { values } => todo!(),
            RawExpressionKind::LiteralHash { values } => todo!(),
            RawExpressionKind::Identifier => todo!(),
            RawExpressionKind::Unary { op, expr } => {
                self.compile_expr(unit, expr, res);
                match op {
                    crate::parser::UnaryOp::OpNot => res.emit(not()),
                    crate::parser::UnaryOp::OpNeg => res.emit(neg()),
                }
            }

            RawExpressionKind::Conditional { condition, positive, negative } => {
                todo!()
            }
            RawExpressionKind::Call { function, arguments } => todo!(),
            RawExpressionKind::IndexExpression { left, idx } => todo!(),
            RawExpressionKind::IllegalExpression => todo!(),
        }
    }

    fn compile_integer(&self, source: &str, res: &mut Bytecode) {
        let int = source.parse::<i64>().unwrap();
        let obj = ByteObj::Int(int);
        let id = res.constants.len();
        res.constants.push(obj);
        emit(cte(id as u16), res)
    }
}

fn emit(op: Operation, res: &mut Bytecode) {
    res.instructions.push(op)
}

struct VmState {
    stack: [ByteObj; 1048],
    sp: usize,
}

impl fmt::Debug for VmState {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self.stack[0..self.sp].to_vec())
    }
}

impl VmState {
    fn push(&mut self, value: ByteObj) {
        self.stack[self.sp] = value;
        self.sp += 1;
    }

    fn pop(&mut self) -> ByteObj {
        assert!(self.sp != 0);
        self.sp -= 1;
        self.stack[self.sp]
    }
}

struct Vm {
    bytecode: Bytecode,
}

impl Vm {
    fn new(bytecode: Bytecode) -> Vm {
        Vm { bytecode }
    }

    fn run(&self, state: &mut VmState) {
        let pool = &self.bytecode.constants;
        for instructions in &self.bytecode.instructions {
            println!("{:?}", state);

            match instructions {
                Operation::Cte(idx) => {
                    let value = pool.get(idx.0 as usize).unwrap();
                    state.push(*value)
                }
                Operation::Add()
                | Operation::Sub()
                | Operation::Mul()
                | Operation::Div()
                | Operation::Gt()
                | Operation::Lt()
                | Operation::Eq()
                | Operation::Neq() => run_binary(instructions, state),

                Operation::Neg() => {
                    let value = state.pop();
                    match value {
                        ByteObj::Int(l) => state.push(bint(-l)),
                        _ => todo!(),
                    }
                }
                Operation::Not() => {
                    let value = state.pop();
                    match value {
                        ByteObj::Bool(l) => state.push(bbool(!l)),
                        _ => todo!(),
                    }
                }
                Operation::Pop() => {
                    state.pop();
                }
            }
        }
    }
}

fn run_binary(op: &Operation, state: &mut VmState) {
    let right = state.pop();
    let left = state.pop();

    let res = match (left, right) {
        (ByteObj::Int(l), ByteObj::Int(r)) => match op {
            Operation::Add() => ByteObj::Int(l + r),
            Operation::Sub() => ByteObj::Int(l - r),
            Operation::Mul() => ByteObj::Int(l * r),
            Operation::Div() => ByteObj::Int(l / r),
            Operation::Gt() => ByteObj::Bool(l > r),
            Operation::Lt() => ByteObj::Bool(l < r),
            Operation::Eq() => ByteObj::Bool(l == r),
            Operation::Neq() => ByteObj::Bool(l != r),
            _ => unreachable!(),
        },

        (ByteObj::Bool(l), ByteObj::Bool(r)) => match op {
            Operation::Eq() => ByteObj::Bool(l == r),
            Operation::Neq() => ByteObj::Bool(l != r),
            _ => unreachable!(),
        },
        _ => unreachable!(),
    };

    state.push(res)
}

#[cfg(test)]
mod tests {
    use crate::{
        bytecode::{add, bbool, bint, cte, ByteObj, Bytecode, Operation},
        parser::{raw_statements, ExprTable},
    };

    use super::{CompUnit, Compiler, Vm, VmState};

    struct VmResult {
        vm: Vm,
        state: VmState,
    }

    fn run(source: &str) -> VmResult {
        let vm = {
            let mut table = ExprTable::new();
            let statements = raw_statements(source, &mut table);
            let unit = CompUnit { source, table, statements };
            let bytecode = Compiler::new().compile(&unit);
            Vm::new(bytecode)
        };

        let mut state = VmState { stack: [ByteObj::Int(0); 1048], sp: 0 };
        let result = vm.run(&mut state);
        VmResult { vm, state }
    }

    fn assert_res(result: VmResult, expected: ByteObj) {
        assert_eq!(result.state.stack[result.state.sp], expected);
    }

    fn assert_code(result: VmResult, expected: Bytecode) {
        assert_eq!(result.vm.bytecode, expected);
    }

    #[test]
    fn compiles_integer_arithmetic() {
        assert_code(
            run("1 + 2"),
            Bytecode {
                constants: vec![bint(1), bint(2)],
                instructions: vec![cte(0), cte(1), add()],
            },
        )
    }

    #[test]
    fn runs_integer_arithmetic() {
        assert_res(run("1 + 2"), bint(3));
        assert_res(run("2 - 1"), bint(1));
        assert_res(run("2 * 2"), bint(4));
        assert_res(run("4 / 2"), bint(2));
        assert_res(run("4 > 2"), bbool(true));
        assert_res(run("4 < 2"), bbool(false));
        assert_res(run("4 == 2"), bbool(false));
        assert_res(run("4 != 2"), bbool(true));
        assert_res(run("true == true"), bbool(true));
        assert_res(run("false != true"), bbool(true));
        assert_res(run("(1 + 2) * (2 + 3)"), bint(15));
        assert_res(run("--2"), bint(2));
        assert_res(run("--2"), bint(2));
        assert_res(run("5 * (2 + 10)"), bint(60));
    }

    #[test]
    fn testtt() {
        assert_code(
            run("1; 2; 3;"),
            Bytecode {
                constants: vec![bint(1), bint(2)],
                instructions: vec![cte(0), cte(1), add()],
            },
        )
    }
}
