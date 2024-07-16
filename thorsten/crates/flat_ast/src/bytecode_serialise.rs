use crate::bytecode;
use crate::bytecode::{BytePosition, ConstantId, Operation};

const CTE: u8 = 0x01;
const ADD: u8 = 0x02;
const SUB: u8 = 0x03;
const MUL: u8 = 0x04;
const DIV: u8 = 0x05;
const GT: u8 = 0x06;
const EQ: u8 = 0x07;
const NEQ: u8 = 0x08;
const NEG: u8 = 0x09;
const NOT: u8 = 0x10;
const POP: u8 = 0x11;
const TRUE: u8 = 0x12;
const FALSE: u8 = 0x13;
const JUMP: u8 = 0x14;
const JUMP_IF: u8 = 0x15;

pub fn serialise<F>(op: &Operation, mut f: F) where F: FnMut(u8) {
    match op {
        Operation::Cte(idx) => {
            f(CTE);
            f((idx.0 >> 8) as u8);
            f(idx.0 as u8);
        }
        Operation::Add() => f(ADD),
        Operation::Sub() => f(SUB),
        Operation::Mul() => f(MUL),
        Operation::Div() => f(DIV),
        Operation::Gt() => f(GT),
        Operation::Eq() => f(EQ),
        Operation::Neq() => f(NEQ),
        Operation::Neg() => f(NEG),
        Operation::Not() => f(NOT),
        Operation::Pop() => f(POP),
        Operation::True() => f(TRUE),
        Operation::False() => f(FALSE),
        Operation::Jump(pos) => {
            f(JUMP);
            f((pos.0 >> 8) as u8);
            f(pos.0 as u8);
        }
        Operation::JumpUnless(pos) => {
            f(JUMP_IF);
            f((pos.0 >> 8) as u8);
            f(pos.0 as u8);
        }
    }
}

pub fn deserialise(ops: &[u8]) -> Operation {
    match ops {
        [CTE, left, right, ..] => bytecode::Operation::Cte(ConstantId(concat_u8(*left, *right))),
        [JUMP, left, right, ..] => bytecode::Operation::Jump(BytePosition(concat_u8(*left, *right))),
        [JUMP_IF, left, right, ..] => bytecode::Operation::JumpUnless(BytePosition(concat_u8(*left, *right))),
        [ADD, ..] => bytecode::Operation::Add(),
        [SUB, ..] => bytecode::Operation::Sub(),
        [MUL, ..] => bytecode::Operation::Mul(),
        [DIV, ..] => bytecode::Operation::Div(),
        [GT, ..] => bytecode::Operation::Gt(),
        [EQ, ..] => bytecode::Operation::Eq(),
        [NEQ, ..] => bytecode::Operation::Neq(),
        [NEG, ..] => bytecode::Operation::Neg(),
        [NOT, ..] => bytecode::Operation::Not(),
        [TRUE, ..] => bytecode::Operation::True(),
        [FALSE, ..] => bytecode::Operation::False(),
        _ => todo!(),
    }
}

fn concat_u8(left: u8, right: u8) -> u16 {
    (left as u16) << 8 | (right as u16)
}

#[cfg(test)]
mod tests {
    use crate::bytecode::{ConstantId, Operation};
    use crate::bytecode_serialise::{deserialise, serialise};

    fn assert_serialises(op: Operation, bytes: &[u8]) {
        fn from_op(op: &Operation) -> Vec<u8> {
            let mut res = vec![];
            serialise(op, |byte| res.push(byte));
            res
        }

        fn to_op(op: &[u8]) -> Operation {
            let op = deserialise(op);
            debug_assert!(op.byte_length() == op.byte_length());
            op
        }

        assert_eq!(from_op(&op), bytes);
        assert_eq!(to_op(bytes), op);
    }

    #[test]
    fn serialises_op_cte() {
        assert_serialises(Operation::Cte(ConstantId(65534)), &[0x01, 0xFF, 0xFE])
    }

    #[test]
    fn serialises_op_add() {
        assert_serialises(Operation::Add(), &[0x02])
    }
}
