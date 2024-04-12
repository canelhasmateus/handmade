#[derive(Debug, PartialEq, Eq)]
pub struct ConstantId(pub u16);

#[derive(Debug, PartialEq, Eq)]
pub enum Operation {
    Cte(ConstantId),
    Add(),
}

impl Operation {
    pub fn len(&self) -> usize {
        match self {
            Operation::Cte(_) => 3,
            Operation::Add() => 1,
        }
    }
}

const CTE: u8 = 0x01;
const ADD: u8 = 0x02;

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
    }
}

pub fn deserialise(ops: &[u8]) -> Operation {
    match ops {
        [CTE, left, right, ..] => cte(concat_u8(*left, *right)),
        [ADD, ..] => add(),
        _ => todo!(),
    }
}

fn concat_u8(left: u8, right: u8) -> u16 {
    (left as u16) << 8 | (right as u16)
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum ByteObj {
    Int(usize),
}

pub fn bint(arg: usize) -> ByteObj {
    ByteObj::Int(arg)
}
pub fn cte(arg: u16) -> Operation {
    Operation::Cte(ConstantId(arg))
}
pub fn add() -> Operation {
    Operation::Add()
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