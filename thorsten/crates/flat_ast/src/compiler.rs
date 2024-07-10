use core::fmt;

use crate::{
    bytecode::{
        add, bbool, bint, Bytecode, ByteObj, cte, div, eq, gt, jump_unless, mul, neg, neq,
        not, op_false, op_true, Operation, pop, sub,
    },
    parser::{
        CompUnit, ExpressionId, RawExpression, RawExpressionKind, RawStatement, RawStatementKind,
        StatementId, UnaryOp,
    },
};
use crate::bytecode::jump;

struct Instructions {
    instructions: Vec<Operation>,
}

struct Compiler {}

impl Compiler {
    pub fn new() -> Compiler {
        Compiler {}
    }

    pub fn compile(&self, unit: &CompUnit) -> Bytecode {
        let mut res = Bytecode { byte_pos: 0, constants: vec![], instructions: vec![] };
        for statement in &unit.statements {
            self.compile_statement(unit, *statement, &mut res)
        }
        res
    }

    fn compile_statement(&self, unit: &CompUnit, statement: StatementId, res: &mut Bytecode) {
        let RawStatement { kind, .. } = unit.statement(statement);
        match kind {
            RawStatementKind::LetStmt { name, expr } => todo!(),
            RawStatementKind::ReturnStmt { expr } => todo!(),
            RawStatementKind::ExprStmt { expr } => {
                self.compile_expr(unit, *expr, res);
                res.emit(pop())
            }
            RawStatementKind::IllegalStatement => todo!(),
            RawStatementKind::EndStatement => {}
        }
    }

    fn compile_expr(&self, unit: &CompUnit, expr: ExpressionId, res: &mut Bytecode) {
        let RawExpression { range, kind } = unit.expression(expr);
        match kind {
            RawExpressionKind::LiteralInteger => {
                let value = unit.int(range).unwrap();
                let id = res.add(bint(value));
                res.emit(Operation::Cte(id));
            }
            RawExpressionKind::LiteralBoolean => match unit.bool(range).unwrap() {
                true => res.emit(op_true()),
                false => res.emit(op_false()),
            },
            RawExpressionKind::Binary { op, left, right } => {
                self.compile_expr(unit, *left, res);
                self.compile_expr(unit, *right, res);
                match op {
                    crate::parser::BinaryOp::Plus => res.emit(add()),
                    crate::parser::BinaryOp::Minus => res.emit(sub()),
                    crate::parser::BinaryOp::Times => res.emit(mul()),
                    crate::parser::BinaryOp::Div => res.emit(div()),
                    crate::parser::BinaryOp::Greater => res.emit(gt()),
                    crate::parser::BinaryOp::Lesser => {
                        res.swap();
                        res.emit(gt())
                    }
                    crate::parser::BinaryOp::Equals => res.emit(eq()),
                    crate::parser::BinaryOp::Differs => res.emit(neq()),
                }
            }
            RawExpressionKind::LiteralString => todo!(),
            RawExpressionKind::Parenthesized { expr } => self.compile_expr(unit, *expr, res),
            RawExpressionKind::LiteralFunction { parameters, body } => todo!(),
            RawExpressionKind::LiteralArray { values } => todo!(),
            RawExpressionKind::LiteralHash { values } => todo!(),
            RawExpressionKind::Identifier => todo!(),
            RawExpressionKind::Unary { op, expr } => {
                self.compile_expr(unit, *expr, res);
                match op {
                    UnaryOp::OpNot => res.emit(not()),
                    UnaryOp::OpNeg => res.emit(neg()),
                }
            }

            RawExpressionKind::Conditional { condition, positive, negative } => {
                self.compile_expr(unit, *condition, res);

                let first_idx = {
                    res.emit(jump_unless(9999));
                    let first_idx = res.index();
                    self.compile_block(unit, positive, res);

                    if let Some(Operation::Pop()) = res.last() {
                        res.pop();
                    }
                    if !negative.is_empty() {
                        res.emit(jump(9999))
                    }

                    first_idx
                };

                res.set(first_idx, jump_unless(res.position().0));

                if !negative.is_empty() {
                    let second_idx = res.index();
                    self.compile_block(unit, negative, res);
                    res.set(second_idx, jump(res.position().0));
                    if let Some(Operation::Pop()) = res.last() {
                        res.pop();
                    }
                }

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
        res.emit(cte(id as u16))
    }

    fn compile_block(&self, unit: &CompUnit, block: &[StatementId], res: &mut Bytecode) {
        for ele in block {
            self.compile_statement(unit, *ele, res);
        }
    }
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

    fn peek(&self) -> Option<ByteObj> {
        if self.sp == 0 {
            None
        } else {
            Some(self.stack[self.sp - 1])
        }
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
        let instructions = &self.bytecode.instructions;
        let mut i = 0;
        while i < instructions.len() {
            println!("{:?}: {:?}", i, state);
            match &instructions[i] {
                Operation::Jump(pos) => {
                    i = pos.0 as usize;
                    continue;
                }
                Operation::JumpUnless(pos) => match state.peek() {
                    Some(ByteObj::Bool(b)) => {
                        if b {
                            i = pos.0 as usize;
                            continue;
                        }
                    }
                    _ => unreachable!(),
                },
                Operation::Cte(idx) => {
                    let value = pool.get(idx.0 as usize).unwrap();
                    state.push(*value)
                }
                Operation::True() => state.push(bbool(true)),
                Operation::False() => state.push(bbool(false)),
                Operation::Add()
                | Operation::Sub()
                | Operation::Mul()
                | Operation::Div()
                | Operation::Gt()
                | Operation::Eq()
                | Operation::Neq() => run_binary(&instructions[i], state),

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
            i += 1;
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
        bytecode::{add, bbool, bint, Bytecode, ByteObj, cte, eq, gt, jump_unless, op_true, pop},
        parser::unit,
    };
    use crate::bytecode::jump;

    use super::{Compiler, Vm, VmState};

    struct VmResult {
        vm: Vm,
        state: VmState,
    }

    fn run(source: &str) -> VmResult {
        let vm = {
            let unit = unit(source);
            let bytecode = Compiler::new().compile(&unit);
            Vm::new(bytecode)
        };

        let mut state = VmState { stack: [ByteObj::Int(0); 1048], sp: 0 };
        vm.run(&mut state);
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
                byte_pos: 8,
                constants: vec![bint(1), bint(2)],
                instructions: vec![cte(0), cte(1), add(), pop()],
            },
        );

        assert_code(
            run("1 > 2"),
            Bytecode {
                byte_pos: 8,
                constants: vec![bint(1), bint(2)],
                instructions: vec![cte(0), cte(1), gt(), pop()],
            },
        );
        assert_code(
            run("1 < 2"),
            Bytecode {
                byte_pos: 14,
                constants: vec![bint(1), bint(2)],
                instructions: vec![cte(1), cte(0), gt(), pop()],
            },
        );
        assert_code(
            run("true == true"),
            Bytecode {
                byte_pos: 4,
                constants: vec![],
                instructions: vec![op_true(), op_true(), eq(), pop()],
            },
        );
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
        assert_res(run("(1 + 2) * (2 + 3)"), bint(15));
        assert_res(run("5 * (2 + 10)"), bint(60));

        assert_res(run("true == true"), bbool(true));
        assert_res(run("false == false"), bbool(true));
        assert_res(run("true != false"), bbool(true));
        assert_res(run("false != true"), bbool(true));
        assert_res(run("true == false"), bbool(false));

        assert_res(run("( 1 < 2 ) == true"), bbool(true));
        assert_res(run("( 1 < 2 ) == false"), bbool(false));
        assert_res(run("( 1 > 2 ) == true"), bbool(false));
        assert_res(run("( 1 > 2 ) == false"), bbool(true));

        assert_res(run("-2"), bint(-2));
        assert_res(run("--2"), bint(2));
        assert_res(run("!true"), bbool(false));
        assert_res(run("!!true"), bbool(true));
        assert_res(run("!false"), bbool(true));
        assert_res(run("!!false"), bbool(false));
    }

    #[test]
    fn compiles_conditionals() {
        assert_code(
            run("if ( true ) { 10 }; 3333"),
            Bytecode {
                byte_pos: 13,
                constants: vec![bint(10), bint(3333)],
                instructions: vec![
                    // 0000
                    op_true(),
                    // 0001
                    jump_unless(7),
                    // 0004
                    cte(0),
                    // 0007
                    pop(),
                    // 0008
                    cte(1),
                    // 0011
                    pop(),
                ],
            },
        );
        assert_code(
            run("if ( true ) { 10 } else { 20 }; 3333"),
            Bytecode {
                byte_pos: 20,
                constants: vec![bint(10), bint(20), bint(3333)],
                instructions: vec![
                    // 0000
                    op_true(),
                    // 0001
                    jump_unless(10),
                    // 0004
                    cte(0),
                    // 0007
                    jump(14),
                    // 0010
                    cte(1),
                    // 0013
                    pop(),
                    // 0014
                    cte(2),
                    // 0017
                    pop(),
                ],
            },
        );
    }
}
