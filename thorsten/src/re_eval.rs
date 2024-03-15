use crate::re_parser::ExprTable;

enum Object {
    Integer(i32),
}

struct VM {
    table: ExprTable,
}

impl VM {
    fn eval_source(source: &str) -> Object {
        parse_statement(source)
    }
}
