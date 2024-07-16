use crate::parser::{CompUnit, ExpressionId, RawExpression, RawExpressionKind, RawStatement, RawStatementKind, StatementId, UnaryOp};

#[derive(Debug, PartialEq, Eq)]
pub struct ConstantId(pub u16);

#[derive(Debug, PartialEq, Eq)]
pub struct BytePosition(pub u16);

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
    Jump(BytePosition),
    JumpUnless(BytePosition),
}

#[derive(Debug, PartialEq, Eq)]
pub struct Bytecode {
    pub byte_pos: usize,
    pub constants: Vec<ByteObj>,
    pub instructions: Vec<Operation>,
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum ByteObj {
    Int(i64),
    Bool(bool),
}

impl Operation {
    pub fn byte_length(&self) -> usize {
        match self {
            Operation::Cte(_) => 3,
            Operation::Jump(_) => 3,
            Operation::JumpUnless(_) => 3,
            __ => 1
        }
    }
}

impl Bytecode {
    pub fn add(&mut self, obj: ByteObj) -> ConstantId {
        self.constants.push(obj);
        let id = self.constants.len() - 1;
        ConstantId(id as u16)
    }

    pub fn set(&mut self, pos: usize, op: Operation) {
        self.instructions[pos] = op;
    }

    pub fn emit(&mut self, op: Operation) {
        self.byte_pos += op.byte_length();
        self.instructions.push(op);
    }

    pub fn index(&self) -> usize {
        return self.instructions.len() - 1
    }

    pub fn position(&self) -> BytePosition {
        return BytePosition(self.byte_pos as u16 - 1)
    }

    pub fn swap(&mut self) {
        let last = self.instructions.pop();
        let middle = self.instructions.pop();
        if let Some(f) = last {
            self.emit(f)
        }
        if let Some(f) = middle {
            self.emit(f)
        }
    }

    pub fn last(&self) -> Option<&Operation> {
        self.instructions.last()
    }

    pub fn previous(&self) -> Option<&Operation> {
        if let [.., previous, _last] = &self.instructions[..] {
            Some(previous)
        } else {
            None
        }
    }

    pub(crate) fn pop(&mut self) -> Option<Operation> {
        self.instructions.pop()
    }
}

pub struct Compiler {}

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
                res.emit(Operation::Pop())
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
                let value = ByteObj::Int(value);

                let id = res.add(value);
                let id = Operation::Cte(id);
                res.emit(id);
            }

            RawExpressionKind::LiteralBoolean => match unit.bool(range).unwrap() {
                true => res.emit(Operation::True()),
                false => res.emit(Operation::False()),
            },
            RawExpressionKind::Binary { op, left, right } => {
                self.compile_expr(unit, *left, res);
                self.compile_expr(unit, *right, res);
                match op {
                    crate::parser::BinaryOp::Plus => res.emit(Operation::Add()),
                    crate::parser::BinaryOp::Minus => res.emit(Operation::Sub()),
                    crate::parser::BinaryOp::Times => res.emit(Operation::Mul()),
                    crate::parser::BinaryOp::Div => res.emit(Operation::Div()),
                    crate::parser::BinaryOp::Greater => res.emit(Operation::Gt()),
                    crate::parser::BinaryOp::Lesser => {
                        res.swap();
                        res.emit(Operation::Gt())
                    }
                    crate::parser::BinaryOp::Equals => res.emit(Operation::Eq()),
                    crate::parser::BinaryOp::Differs => res.emit(Operation::Neq()),
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
                    UnaryOp::OpNot => res.emit(Operation::Not()),
                    UnaryOp::OpNeg => res.emit(Operation::Neg()),
                }
            }

            RawExpressionKind::Conditional { condition, positive, negative } => {
                self.compile_expr(unit, *condition, res);

                let first_idx = {
                    res.emit(Operation::JumpUnless(BytePosition(9999)));
                    let first_idx = res.index();
                    self.compile_block(unit, positive, res);

                    if let Some(Operation::Pop()) = res.last() {
                        res.pop();
                    }
                    if !negative.is_empty() {
                        res.emit(Operation::Jump(BytePosition(9999)))
                    }

                    first_idx
                };

                res.set(first_idx, Operation::JumpUnless(res.position()));

                if !negative.is_empty() {
                    let second_idx = res.index();
                    self.compile_block(unit, negative, res);
                    res.set(second_idx, Operation::Jump(res.position()));
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
        res.emit(Operation::Cte(ConstantId(id as u16)))
    }

    fn compile_block(&self, unit: &CompUnit, block: &[StatementId], res: &mut Bytecode) {
        for ele in block {
            self.compile_statement(unit, *ele, res);
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::bytecode::{Bytecode, ByteObj, BytePosition, Compiler, ConstantId, Operation};
    use crate::parser::unit;

    fn bint(arg: i64) -> ByteObj {
        ByteObj::Int(arg)
    }

    fn bbool(arg: bool) -> ByteObj {
        ByteObj::Bool(arg)
    }

    fn cte(arg: u16) -> Operation {
        Operation::Cte(ConstantId(arg))
    }

    fn add() -> Operation {
        Operation::Add()
    }

    fn sub() -> Operation {
        Operation::Sub()
    }

    fn mul() -> Operation {
        Operation::Mul()
    }

    fn div() -> Operation {
        Operation::Div()
    }

    fn gt() -> Operation {
        Operation::Gt()
    }

    fn eq() -> Operation {
        Operation::Eq()
    }

    fn neq() -> Operation {
        Operation::Neq()
    }

    fn neg() -> Operation {
        Operation::Neg()
    }

    fn not() -> Operation {
        Operation::Not()
    }

    fn pop() -> Operation {
        Operation::Pop()
    }

    fn op_true() -> Operation {
        Operation::True()
    }

    fn op_false() -> Operation {
        Operation::False()
    }

    fn jump(pos: u16) -> Operation {
        Operation::Jump(BytePosition(pos))
    }

    fn jump_unless(pos: u16) -> Operation {
        Operation::JumpUnless(BytePosition(pos))
    }

    fn assert_code(source: &str, expected: Bytecode) {
        let unit = unit(source);
        let bytecode = Compiler::new().compile(&unit);
        assert_eq!(bytecode, expected);
    }

    #[test]
    fn compiles_integer_arithmetic() {
        assert_code(
            "1 + 2",
            Bytecode {
                byte_pos: 8,
                constants: vec![bint(1), bint(2)],
                instructions: vec![cte(0), cte(1), add(), pop()],
            },
        );

        assert_code(
            "1 > 2",
            Bytecode {
                byte_pos: 8,
                constants: vec![bint(1), bint(2)],
                instructions: vec![cte(0), cte(1), gt(), pop()],
            },
        );
        assert_code(
            "1 < 2",
            Bytecode {
                byte_pos: 14,
                constants: vec![bint(1), bint(2)],
                instructions: vec![cte(1), cte(0), gt(), pop()],
            },
        );
        assert_code(
            "true == true",
            Bytecode {
                byte_pos: 4,
                constants: vec![],
                instructions: vec![op_true(), op_true(), eq(), pop()],
            },
        );
    }

    #[test]
    fn compiles_conditionals() {
        assert_code(
            "if ( true ) { 10 }; 3333",
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
            "if ( true ) { 10 } else { 20 }; 3333",
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