// #![feature(test)]

mod eval;
mod lexer;
mod parser;
mod flat_eval;
mod flat_lexer;
mod flat_parser;

fn main() {}

#[cfg(test)]
mod tests {
    // extern crate test;
    // use test::Bencher;
    // #[bench]
    // fn bench_suite(b: &mut Bencher) {
    //     let input = "
    //     let x = 5;
    //     let y = 10;
    //     let foobar = 838383;
    //     return 5;
    //     return 10;
    //     return 993322;
    //     name;
    //     name
    //     5;
    //     5
    //     !5;
    //     -15;
    //     5 + 5;
    //     5 - 5;
    //     5 * 5;
    //     5 / 5;
    //     5 > 5;
    //     5 < 5;
    //     5 == 5;
    //     -a * b;
    //     !-a;
    //     a + b + c;
    //     true;
    //     false;
    //     let foobar = true;
    //     let barfoo = false;
    //     1 + (2 + 3) + 4;
    //     (5 + 5) * 2;
    //     2 / (5 + 5);
    //     -(5 + 5);
    //     !(true == true);
    //     ";
    //     let mut parser = crate::parser::Parser::from(input);
    //     b.iter(|| {
    //         parser.next_statement();
    //         parser.next_statement();
    //         parser.next_statement();
    //         parser.next_statement();
    //         parser.next_statement();
    //         parser.next_statement();
    //         parser.next_statement();
    //         parser.next_statement();
    //         parser.next_statement();
    //         parser.next_statement();
    //         parser.next_statement();
    //         parser.next_statement();
    //         parser.next_statement();
    //         parser.next_statement();
    //         parser.next_statement();
    //         parser.next_statement();
    //         parser.next_statement();
    //         parser.next_statement();
    //         parser.next_statement();
    //         parser.next_statement();
    //         parser.next_statement();
    //         parser.next_statement();
    //         parser.next_statement();
    //         parser.next_statement();
    //         parser.next_statement();
    //         parser.next_statement();
    //         parser.next_statement();
    //         parser.next_statement();
    //         parser.next_statement();
    //         parser.next_statement();
    //         parser.next_statement();
    //     });
    // }
}
