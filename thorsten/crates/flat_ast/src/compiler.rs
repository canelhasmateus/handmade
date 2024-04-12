use crate::{
    bytecode::{
        self, add, bbool, bint, cte, div, eq, gt, lt, mul, neq, sub, ByteObj, Bytecode, ConstantId,
        Operation,
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
            RawStatementKind::ExprStmt { expr } => self.compile_expr(unit, &expr, res),
            RawStatementKind::IllegalStatement => todo!(),
            RawStatementKind::EndStatement => todo!(),
        }
    }

    fn compile_expr(&self, unit: &CompUnit, expr: &ExpressionId, res: &mut Bytecode) {
        let RawExpression { range, kind } = unit.table.get_expression(&expr);
        match kind {
            RawExpressionKind::LiteralInteger => {
                let value = (&unit.source[range]).parse::<usize>().unwrap();
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
            RawExpressionKind::Parenthesized { expr } => todo!(),
            RawExpressionKind::LiteralFunction { parameters, body } => todo!(),
            RawExpressionKind::LiteralArray { values } => todo!(),
            RawExpressionKind::LiteralHash { values } => todo!(),
            RawExpressionKind::Identifier => todo!(),
            RawExpressionKind::Unary { op, expr } => todo!(),
            RawExpressionKind::Conditional { condition, positive, negative } => {
                todo!()
            }
            RawExpressionKind::Call { function, arguments } => todo!(),
            RawExpressionKind::IndexExpression { left, idx } => todo!(),
            RawExpressionKind::IllegalExpression => todo!(),
        }
    }

    fn compile_integer(&self, source: &str, res: &mut Bytecode) {
        let int = source.parse::<usize>().unwrap();
        let obj = ByteObj::Int(int);
        let id = res.constants.len();
        res.constants.push(obj);
        emit(cte(id as u16), res)
    }
}

fn emit(op: Operation, res: &mut Bytecode) {
    res.instructions.push(op)
}

#[derive(Debug)]
struct VmState {
    stack: [ByteObj; 1048],
    sp: usize,
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

    fn run(&self, state: &mut VmState) -> ByteObj {
        let pool = &self.bytecode.constants;
        for instructions in &self.bytecode.instructions {
            println!("{:?} {:?}", state.sp, &state.stack[..state.sp]);

            match instructions {
                Operation::Cte(idx) => {
                    let value = pool.get(idx.0 as usize).unwrap();
                    state.push(*value)
                }
                Operation::Add() => {
                    let right = state.pop();
                    let left = state.pop();
                    match (left, right) {
                        (ByteObj::Int(l), ByteObj::Int(r)) => state.push(bint(l + r)),
                        _ => todo!(),
                    }
                }
                Operation::Sub() => {
                    let right = state.pop();
                    let left = state.pop();
                    match (left, right) {
                        (ByteObj::Int(l), ByteObj::Int(r)) => state.push(bint(l - r)),
                        _ => todo!(),
                    }
                }
                Operation::Mul() => {
                    let right = state.pop();
                    let left = state.pop();
                    match (left, right) {
                        (ByteObj::Int(l), ByteObj::Int(r)) => state.push(bint(l * r)),
                        _ => todo!(),
                    }
                }
                Operation::Div() => {
                    let right = state.pop();
                    let left = state.pop();
                    match (left, right) {
                        (ByteObj::Int(l), ByteObj::Int(r)) => state.push(bint(l / r)),
                        _ => todo!(),
                    }
                }
                Operation::Gt() => {
                    let right = state.pop();
                    let left = state.pop();
                    match (left, right) {
                        (ByteObj::Int(l), ByteObj::Int(r)) => state.push(bbool(l > r)),
                        _ => todo!(),
                    }
                }
                Operation::Lt() => {
                    let right = state.pop();
                    let left = state.pop();
                    match (left, right) {
                        (ByteObj::Int(l), ByteObj::Int(r)) => state.push(bbool(l < r)),
                        _ => todo!(),
                    }
                }
                Operation::Eq() => {
                    let right = state.pop();
                    let left = state.pop();
                    match (left, right) {
                        (ByteObj::Int(l), ByteObj::Int(r)) => state.push(bbool(l == r)),
                        (ByteObj::Bool(l), ByteObj::Bool(r)) => state.push(bbool(l == r)),
                        _ => todo!(),
                    }
                }
                Operation::Neq() => {
                    let right = state.pop();
                    let left = state.pop();
                    match (left, right) {
                        (ByteObj::Int(l), ByteObj::Int(r)) => state.push(bbool(l != r)),
                        (ByteObj::Bool(l), ByteObj::Bool(r)) => state.push(bbool(l != r)),
                        _ => todo!(),
                    }
                }
            }
        }
        state.pop()
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        bytecode::{add, bbool, bint, cte, ByteObj, Bytecode, Operation},
        parser::{raw_statements, ExprTable},
    };

    use super::{CompUnit, Compiler, Vm, VmState};

    fn run(source: &str) -> ByteObj {
        let bytecode = compile_source(source);
        let vm = Vm::new(bytecode);
        let mut state = VmState { stack: [ByteObj::Int(0); 1048], sp: 0 };
        vm.run(&mut state)
    }

    fn compile_source(source: &str) -> Bytecode {
        let mut table = ExprTable::new();
        let statements = raw_statements(source, &mut table);
        let unit = CompUnit { source, table, statements };
        Compiler::new().compile(&unit)
    }

    #[test]
    fn compiles_integer_arithmetic() {
        assert_eq!(
            compile_source("1 + 2"),
            Bytecode {
                constants: vec!(bint(1), bint(2)),
                instructions: vec!(cte(0), cte(1), add())
            }
        )
    }

    #[test]
    fn runs_integer_arithmetic() {
        assert_eq!(run("1 + 2"), bint(3));
        assert_eq!(run("2 - 1"), bint(1));
        assert_eq!(run("2 * 2"), bint(4));
        assert_eq!(run("4 / 2"), bint(2));
        assert_eq!(run("4 > 2"), bbool(true));
        assert_eq!(run("4 < 2"), bbool(false));
        assert_eq!(run("4 == 2"), bbool(false));
        assert_eq!(run("4 != 2"), bbool(true));
        assert_eq!(run("true == true"), bbool(true));
        assert_eq!(run("false != true"), bbool(true));
    }
}
