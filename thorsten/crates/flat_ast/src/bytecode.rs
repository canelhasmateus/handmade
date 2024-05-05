#[derive(Debug, PartialEq, Eq)]
pub struct ConstantId(pub u16);

#[derive(Debug, PartialEq, Eq)]
pub enum Operation {
    Cte(ConstantId),
    Add(),
    Sub(),
    Mul(),
    Div(),
    Gt(),
    Eq(),
    Neq(),
    Neg(),
    Not(),
    Pop(),
    True(),
    False(),
}

impl Operation {
    pub fn len(&self) -> usize {
        match self {
            Operation::Cte(_) => 3,
            Operation::Add() => 1,
            Operation::Sub() => 1,
            Operation::Mul() => 1,
            Operation::Div() => 1,
            Operation::Gt() => 1,
            Operation::Eq() => 1,
            Operation::Neq() => 1,
            Operation::Neg() => 1,
            Operation::Not() => 1,
            Operation::Pop() => 1,
            Operation::True() => 1,
            Operation::False() => 1,
        }
    }
}

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

pub fn serialise<F>(op: &Operation, mut f: F)
where
    F: FnMut(u8),
{
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
    }
}

pub fn deserialise(ops: &[u8]) -> Operation {
    match ops {
        [CTE, left, right, ..] => cte(concat_u8(*left, *right)),
        [ADD, ..] => add(),
        [SUB, ..] => sub(),
        [MUL, ..] => mul(),
        [DIV, ..] => div(),
        [GT, ..] => gt(),
        [EQ, ..] => eq(),
        [NEQ, ..] => neq(),
        [NEG, ..] => neg(),
        [NOT, ..] => not(),
        [TRUE, ..] => op_true(),
        [FALSE, ..] => op_false(),
        _ => todo!(),
    }
}

fn concat_u8(left: u8, right: u8) -> u16 {
    (left as u16) << 8 | (right as u16)
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum ByteObj {
    Int(i64),
    Bool(bool),
}

pub fn bint(arg: i64) -> ByteObj {
    ByteObj::Int(arg)
}
pub fn bbool(arg: bool) -> ByteObj {
    ByteObj::Bool(arg)
}

pub fn cte(arg: u16) -> Operation {
    Operation::Cte(ConstantId(arg))
}
pub fn add() -> Operation {
    Operation::Add()
}
pub fn sub() -> Operation {
    Operation::Sub()
}
pub fn mul() -> Operation {
    Operation::Mul()
}
pub fn div() -> Operation {
    Operation::Div()
}
pub fn gt() -> Operation {
    Operation::Gt()
}
pub fn eq() -> Operation {
    Operation::Eq()
}
pub fn neq() -> Operation {
    Operation::Neq()
}
pub fn neg() -> Operation {
    Operation::Neg()
}
pub fn not() -> Operation {
    Operation::Not()
}
pub fn pop() -> Operation {
    Operation::Pop()
}
pub fn op_true() -> Operation {
    Operation::True()
}
pub fn op_false() -> Operation {
    Operation::False()
}

#[derive(Debug, PartialEq, Eq)]
pub struct Bytecode {
    pub constants: Vec<ByteObj>,
    pub instructions: Vec<Operation>,
}

impl Bytecode {
    pub fn add(&mut self, obj: ByteObj) -> ConstantId {
        self.constants.push(obj);
        let id = self.constants.len() - 1;
        ConstantId(id as u16)
    }
    pub fn emit(&mut self, op: Operation) {
        self.instructions.push(op)
    }

    pub fn swap(&mut self) {
        let last = self.instructions.pop();
        let middle = self.instructions.pop();
        last.map(|f| self.emit(f));
        middle.map(|f| self.emit(f));
    }
}

#[cfg(test)]
mod tests {

    use crate::bytecode::{deserialise, serialise, ConstantId, Operation};

    fn assert_serialises(op: Operation, bytes: &[u8]) {
        fn from_op(op: &Operation) -> Vec<u8> {
            let mut res = vec![];
            serialise(op, |byte| res.push(byte));
            res
        }

        fn to_op(op: &[u8]) -> Operation {
            let op = deserialise(op);
            debug_assert!(op.len() == op.len());
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
