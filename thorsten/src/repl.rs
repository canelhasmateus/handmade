use std::io::{Stdin, Stdout, Write};

use crate::lexer::{Lexer, Token};

pub struct Repl {
    pub input: Stdin,
    pub output: Stdout,
}

impl Repl {
    pub fn run(mut self) {
        let prompt = ">> ";

        loop {
            self.output
                .write_all(prompt.as_ref())
                .expect("TODO: panic message");
            self.output.flush().expect("TODO: panic message");

            let mut input = String::new();
            let result = self.input.read_line(&mut input).unwrap();
            let mut lexer = Lexer::from(input.as_ref());

            loop {
                let token = lexer.next_token();
                if token == Token::Eof {
                    break;
                }

                self.output
                    .write(format!("{:?}\n", token).as_ref())
                    .expect("TODO: panic message");
                self.output.flush().expect("TODO: panic message");
            }
        }
    }
}
