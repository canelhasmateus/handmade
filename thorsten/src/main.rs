use std::io::{stdin, stdout};

use crate::repl::Repl;

mod lexer;
mod repl;
mod parser;

fn main() {
    let repl = Repl {
        input: stdin(),
        output: stdout(),
    };
    repl.run();
}
