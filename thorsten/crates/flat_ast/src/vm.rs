use core::fmt;

use crate::bytecode::{
    Bytecode, ByteObj
    , Operation,
};

struct VmState {
    stack: [ByteObj; 1048],
    sp: usize,
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
                Operation::True() => state.push(ByteObj::Bool(true)),
                Operation::False() => state.push(ByteObj::Bool(false)),
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
                        ByteObj::Int(l) => state.push(ByteObj::Int(-l)),
                        _ => todo!(),
                    }
                }
                Operation::Not() => {
                    let value = state.pop();
                    match value {
                        ByteObj::Bool(l) => state.push(ByteObj::Bool(!l)),
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
    use crate::bytecode::{Bytecode, ByteObj, Compiler};
    use crate::parser::unit;
    use crate::vm::{Vm, VmState};

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
    fn runs_integer_arithmetic() {
        assert_res(run("1 + 2"), ByteObj::Int(3));
        assert_res(run("2 - 1"), ByteObj::Int(1));
        assert_res(run("2 * 2"), ByteObj::Int(4));
        assert_res(run("4 / 2"), ByteObj::Int(2));
        assert_res(run("4 > 2"), ByteObj::Bool(true));
        assert_res(run("4 < 2"), ByteObj::Bool(false));
        assert_res(run("4 == 2"), ByteObj::Bool(false));
        assert_res(run("4 != 2"), ByteObj::Bool(true));
        assert_res(run("(1 + 2) * (2 + 3)"), ByteObj::Int(15));
        assert_res(run("5 * (2 + 10)"), ByteObj::Int(60));

        assert_res(run("true == true"), ByteObj::Bool(true));
        assert_res(run("false == false"), ByteObj::Bool(true));
        assert_res(run("true != false"), ByteObj::Bool(true));
        assert_res(run("false != true"), ByteObj::Bool(true));
        assert_res(run("true == false"), ByteObj::Bool(false));

        assert_res(run("( 1 < 2 ) == true"), ByteObj::Bool(true));
        assert_res(run("( 1 < 2 ) == false"), ByteObj::Bool(false));
        assert_res(run("( 1 > 2 ) == true"), ByteObj::Bool(false));
        assert_res(run("( 1 > 2 ) == false"), ByteObj::Bool(true));

        assert_res(run("-2"), ByteObj::Int(-2));
        assert_res(run("--2"), ByteObj::Int(2));
        assert_res(run("!true"), ByteObj::Bool(false));
        assert_res(run("!!true"), ByteObj::Bool(true));
        assert_res(run("!false"), ByteObj::Bool(true));
        assert_res(run("!!false"), ByteObj::Bool(false));
    }
}
