// use std::fmt::Binary;
//
// use crate::lexer::{Identifier, Lexer, TokenKind};
// use crate::lexer::TokenKind::{Eof, Semicolon};
// use crate::parser::Expression::{ExprError, ExprPrefix};
// use crate::parser::Node::{StmtError, StmtExpr, StmtLet, StmtReturn};
// use crate::parser::Precedence::{Lowest, Prefix};
// use crate::parser::PrefixKind::{Minus, Not};
//
// #[derive(Debug, Eq, PartialEq, Clone)]
// pub enum Node {
//     StmtError { current: TokenKind, peek: TokenKind },
//     StmtLet { name: Identifier, value: Expression },
//     StmtReturn { value: Expression },
//     StmtExpr { value: Expression },
// }
//
// #[derive(Debug, Eq, PartialEq, Clone)]
// pub enum Expression {
//     ExprInteger {
//         value: i32,
//     },
//     ExprBool {
//         value: bool,
//     },
//     ExprIdent {
//         name: Identifier,
//     },
//     ExprPrefix {
//         kind: PrefixKind,
//         value: Box<Expression>,
//     },
//     ExprBinary {
//         left: Box<Expression>,
//         right: Box<Expression>,
//         kind: ExprKind,
//     },
//     ExprCond {
//         condition: Box<Expression>,
//         positive: Box<Expression>,
//         negative: Box<Expression>,
//     },
//     ExprError {
//         value: Vec<TokenKind>,
//     },
// }
//
// #[derive(Debug, Eq, PartialEq, Clone)]
// pub enum PrefixKind {
//     Not,
//     Minus,
// }
//
// #[derive(Debug, Eq, PartialEq, Clone)]
// pub enum ExprKind {
//     Plus,
//     Minus,
//     Times,
//     Div,
//     Greater,
//     Lesser,
//     Equals,
//     Differs,
// }
//
// #[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
// enum Precedence {
//     Lowest,
//     Equals,
//     LesserGreater,
//     Sum,
//     Product,
//     Prefix,
//     Call,
// }
//
// struct Parser {
//     lexer: Lexer,
//     current: TokenKind,
//     peek: TokenKind,
// }
//
// impl Parser {
//     fn next_token(&mut self) -> TokenKind {
//         if self.current == Eof && self.peek == Eof {
//             self.current = self.lexer.next_token();
//             self.peek = self.lexer.next_token();
//         } else {
//             self.current = self.peek.clone(); // todo
//             self.peek = self.lexer.next_token();
//         }
//         return self.current.clone();
//     }
//
//     fn next_statement(&mut self) -> Node {
//         self.next_token();
//         let res = match self.current.clone() {
//             TokenKind::Return => {
//                 self.next_token();
//                 StmtReturn {
//                     value: self.next_prefix(Lowest),
//                 }
//             }
//
//             TokenKind::Let => {
//                 let ident = self.next_token();
//                 let equals = self.next_token();
//                 let expr = self.next_token();
//                 match (ident, equals, expr) {
//                     (TokenKind::Ident { name }, TokenKind::Assign, _) => StmtLet {
//                         name,
//                         value: self.next_prefix(Lowest),
//                     },
//                     (ident, assign, expr) => StmtError { current: ident, peek: assign }
//                 }
//             }
//
//             _ => StmtExpr { value: self.next_prefix(Lowest) }
//         };
//
//         if self.peek == TokenKind::Semicolon {
//             self.next_token();
//         }
//         return res;
//     }
//
//     fn next_prefix(&mut self, precedence: Precedence) -> Expression {
//         let mut left = match self.current.clone() {
//             TokenKind::True => Expression::ExprBool { value: true },
//             TokenKind::False => Expression::ExprBool { value: false },
//             TokenKind::Ident { name } => Expression::ExprIdent { name },
//             TokenKind::Int { value } => Expression::ExprInteger { value },
//             TokenKind::Lparen => {
//                 self.next_token();
//                 let expression = self.next_prefix(Lowest);
//                 let rp = self.next_token();
//                 match (expression, rp) {
//                     (exp, TokenKind::Rparen) => exp,
//                     (exp, tk) => ExprError { value: vec![tk.clone()] }
//                 }
//             }
//
//             TokenKind::Bang => {
//                 self.next_token();
//                 ExprPrefix {
//                     kind: Not,
//                     value: self.next_prefix(Prefix).into(),
//                 }
//             }
//
//             TokenKind::Minus => {
//                 self.next_token();
//                 ExprPrefix {
//                     kind: Minus,
//                     value: self.next_prefix(Prefix).into(),
//                 }
//             }
//             _ => ExprError {
//                 value: vec![self.current.clone()],
//             }
//         };
//
//         while self.peek != Semicolon && precedence < Precedence::from(&self.peek) {
//             left = self.next_infix(left)
//         }
//
//         return left;
//     }
//
//     fn next_infix(&mut self, left: Expression) -> Expression {
//         match ExprKind::try_from(&self.peek) {
//             Ok(kind) => {
//                 let precedence = Precedence::from(&self.peek);
//                 self.next_token();
//                 self.next_token();
//                 Expression::ExprBinary {
//                     left: left.into(),
//                     right: self.next_prefix(precedence).into(),
//                     kind,
//                 }
//             }
//             _ => left,
//         }
//     }
// }
//
// impl From<&TokenKind> for Precedence {
//     fn from(value: &TokenKind) -> Self {
//         match value {
//             TokenKind::Plus => Precedence::Sum,
//             TokenKind::Minus => Precedence::Sum,
//             TokenKind::Asterisk => Precedence::Product,
//             TokenKind::Slash => Precedence::Product,
//             TokenKind::Lt => Precedence::LesserGreater,
//             TokenKind::Gt => Precedence::LesserGreater,
//             TokenKind::Equals => Precedence::Equals,
//             TokenKind::Differs => Precedence::Equals,
//             _ => Precedence::Lowest,
//         }
//     }
// }
//
// impl TryFrom<&TokenKind> for ExprKind {
//     type Error = ();
//     fn try_from(value: &TokenKind) -> Result<Self, Self::Error> {
//         match *value {
//             TokenKind::Plus => Ok(ExprKind::Plus),
//             TokenKind::Minus => Ok(ExprKind::Minus),
//             TokenKind::Slash => Ok(ExprKind::Div),
//             TokenKind::Asterisk => Ok(ExprKind::Times),
//             TokenKind::Equals => Ok(ExprKind::Equals),
//             TokenKind::Differs => Ok(ExprKind::Differs),
//             TokenKind::Lt => Ok(ExprKind::Lesser),
//             TokenKind::Gt => Ok(ExprKind::Greater),
//             _ => Err(()),
//         }
//     }
// }
//
// impl From<&str> for Parser {
//     fn from(value: &str) -> Self {
//         return Parser {
//             lexer: Lexer::from(value),
//             current: TokenKind::Eof,
//             peek: TokenKind::Eof,
//         };
//     }
// }
//
// #[cfg(test)]
// mod tests {
//     use crate::parser::Expression::{ExprBinary, ExprBool, ExprIdent, ExprInteger, ExprPrefix};
//     use crate::parser::Node::StmtLet;
//
//     use super::*;
//
//     #[test]
//     fn parser_initialization() {
//         let mut parser = Parser::from("1 + 2");
//
//         assert_eq!(parser.current, TokenKind::Eof);
//         assert_eq!(parser.peek, TokenKind::Eof);
//
//         parser.next_token();
//         assert_eq!(parser.current, TokenKind::Int { value: 1 });
//         assert_eq!(parser.peek, TokenKind::Plus);
//
//         parser.next_token();
//         assert_eq!(parser.current, TokenKind::Plus);
//         assert_eq!(parser.peek, TokenKind::Int { value: 2 });
//
//         parser.next_token();
//         assert_eq!(parser.current, TokenKind::Int { value: 2 });
//         assert_eq!(parser.peek, TokenKind::Eof);
//     }
//
//     #[test]
//     fn let_statements() {
//         let input = "
//         let x = 5;
//         let y = 10;
//         let foobar = 838383;
//         ";
//
//         let mut parser = Parser::from(input);
//
//         assert_eq!(
//             parser.next_statement(),
//             StmtLet {
//                 name: Identifier::from("x"),
//                 value: ExprInteger { value: 5 }
//             },
//         );
//         assert_eq!(
//             parser.next_statement(),
//             StmtLet {
//                 name: Identifier::from("y"),
//                 value: ExprInteger { value: 10 }
//             },
//         );
//         assert_eq!(
//             parser.next_statement(),
//             StmtLet {
//                 name: Identifier::from("foobar"),
//                 value: ExprInteger { value: 838383 }
//             }
//         );
//     }
//
//     #[test]
//     fn return_statements() {
//         let input = "
//         return 5;
//         return 10;
//         return 993322;
//         ";
//
//         let mut parser = Parser::from(input);
//
//         assert_eq!(
//             parser.next_statement(),
//             StmtReturn {
//                 value: ExprInteger { value: 5 }
//             },
//         );
//         assert_eq!(
//             parser.next_statement(),
//             StmtReturn {
//                 value: ExprInteger { value: 10 }
//             },
//         );
//         assert_eq!(
//             parser.next_statement(),
//             StmtReturn {
//                 value: ExprInteger { value: 993322 }
//             },
//         );
//     }
//
//     #[test]
//     fn expression_statements() {
//         let input = "
//         name;
//         name
//         5;
//         5
//         !5;
//         -15;
//         ";
//
//         let mut parser = Parser::from(input);
//
//         assert_eq!(
//             parser.next_statement(),
//             StmtExpr {
//                 value: ExprIdent {
//                     name: Identifier::from("name")
//                 }
//             },
//         );
//         assert_eq!(
//             parser.next_statement(),
//             StmtExpr {
//                 value: ExprIdent {
//                     name: Identifier::from("name")
//                 }
//             },
//         );
//
//         assert_eq!(
//             parser.next_statement(),
//             StmtExpr {
//                 value: ExprInteger { value: 5 }
//             },
//         );
//         assert_eq!(
//             parser.next_statement(),
//             StmtExpr {
//                 value: ExprInteger { value: 5 }
//             },
//         );
//
//         assert_eq!(
//             parser.next_statement(),
//             StmtExpr {
//                 value: ExprPrefix {
//                     kind: PrefixKind::Not,
//                     value: Box::from(ExprInteger { value: 5 })
//                 }
//             },
//         );
//         assert_eq!(
//             parser.next_statement(),
//             StmtExpr {
//                 value: ExprPrefix {
//                     kind: PrefixKind::Minus,
//                     value: Box::from(ExprInteger { value: 15 })
//                 }
//             },
//         );
//     }
//
//     #[test]
//     fn infix_operators() {
//         let input = "
//         5 + 5;
//         5 - 5;
//         5 * 5;
//         5 / 5;
//         5 > 5;
//         5 < 5;
//         5 == 5;
//         5 != 5;
//         ";
//
//         let mut parser = Parser::from(input);
//
//         assert_eq!(
//             parser.next_statement(),
//             StmtExpr {
//                 value: ExprBinary {
//                     kind: ExprKind::Plus,
//                     left: ExprInteger { value: 5 }.into(),
//                     right: ExprInteger { value: 5 }.into()
//                 }
//             },
//         );
//
//         assert_eq!(
//             parser.next_statement(),
//             StmtExpr {
//                 value: ExprBinary {
//                     kind: ExprKind::Minus,
//                     left: ExprInteger { value: 5 }.into(),
//                     right: ExprInteger { value: 5 }.into()
//                 }
//             },
//         );
//
//         assert_eq!(
//             parser.next_statement(),
//             StmtExpr {
//                 value: ExprBinary {
//                     kind: ExprKind::Times,
//                     left: ExprInteger { value: 5 }.into(),
//                     right: ExprInteger { value: 5 }.into()
//                 }
//             },
//         );
//
//         assert_eq!(
//             parser.next_statement(),
//             StmtExpr {
//                 value: ExprBinary {
//                     kind: ExprKind::Div,
//                     left: ExprInteger { value: 5 }.into(),
//                     right: ExprInteger { value: 5 }.into()
//                 }
//             },
//         );
//
//         assert_eq!(
//             parser.next_statement(),
//             StmtExpr {
//                 value: ExprBinary {
//                     kind: ExprKind::Greater,
//                     left: ExprInteger { value: 5 }.into(),
//                     right: ExprInteger { value: 5 }.into()
//                 }
//             },
//         );
//
//         assert_eq!(
//             parser.next_statement(),
//             StmtExpr {
//                 value: ExprBinary {
//                     kind: ExprKind::Lesser,
//                     left: ExprInteger { value: 5 }.into(),
//                     right: ExprInteger { value: 5 }.into()
//                 }
//             },
//         );
//
//         assert_eq!(
//             parser.next_statement(),
//             StmtExpr {
//                 value: ExprBinary {
//                     kind: ExprKind::Equals,
//                     left: ExprInteger { value: 5 }.into(),
//                     right: ExprInteger { value: 5 }.into()
//                 }
//             },
//         );
//
//         assert_eq!(
//             parser.next_statement(),
//             StmtExpr {
//                 value: ExprBinary {
//                     kind: ExprKind::Differs,
//                     left: ExprInteger { value: 5 }.into(),
//                     right: ExprInteger { value: 5 }.into()
//                 }
//             },
//         );
//     }
//
//     #[test]
//     fn infix_precedence() {
//         let input = "
//         -a * b;
//         !-a;
//         a + b + c;
//         ";
//
//         let mut parser = Parser::from(input);
//
//         assert_eq!(
//             parser.next_statement(),
//             StmtExpr {
//                 value: ExprBinary {
//                     left: ExprPrefix {
//                         kind: PrefixKind::Minus,
//                         value: ExprIdent {
//                             name: Identifier::from("a")
//                         }
//                             .into(),
//                     }
//                         .into(),
//                     kind: ExprKind::Times,
//                     right: ExprIdent {
//                         name: Identifier::from("b")
//                     }
//                         .into(),
//                 }
//             }
//         );
//
//         assert_eq!(
//             parser.next_statement(),
//             StmtExpr {
//                 value: ExprPrefix {
//                     kind: PrefixKind::Not,
//                     value: ExprPrefix {
//                         kind: PrefixKind::Minus,
//                         value: ExprIdent {
//                             name: Identifier::from("a")
//                         }
//                             .into(),
//                     }
//                         .into(),
//                 }
//             }
//         );
//         assert_eq!(
//             parser.next_statement(),
//             StmtExpr {
//                 value: ExprBinary {
//                     left: Box::from(ExprBinary {
//                         kind: ExprKind::Plus,
//                         left: ExprIdent { name: Identifier::from("a") }.into(),
//                         right: ExprIdent { name: Identifier::from("b") }.into(),
//                     }),
//                     kind: ExprKind::Plus,
//                     right: Box::from(ExprIdent { name: Identifier::from("c") }
//                     ),
//                 }
//             }
//         );
//     }
//
//     #[test]
//     fn boolean_literals() {
//         let input = "
//         true;
//         false;
//         let foobar = true;
//         let barfoo = false;
//         ";
//
//         let mut parser = Parser::from(input);
//
//         assert_eq!(
//             parser.next_statement(),
//             StmtExpr { value: ExprBool { value: true } }
//         );
//         assert_eq!(
//             parser.next_statement(),
//             StmtExpr { value: ExprBool { value: false } }
//         );
//
//         assert_eq!(
//             parser.next_statement(),
//             StmtLet {
//                 name: Identifier::from("foobar"),
//                 value: ExprBool { value: true },
//             }
//         );
//         assert_eq!(
//             parser.next_statement(),
//             StmtLet {
//                 name: Identifier::from("barfoo"),
//                 value: ExprBool { value: false },
//             }
//         );
//     }
//
//     #[test]
//     fn grouped_expressions() {
//         let input = "
//         1 + (2 + 3) + 4;
//         (5 + 5) * 2;
//         2 / (5 + 5);
//         -(5 + 5);
//         !(true == true);
//         ";
//
//         let mut parser = Parser::from(input);
//
//         assert_eq!(
//             parser.next_statement(),
//             StmtExpr {
//                 value: ExprBinary {
//                     kind: ExprKind::Plus,
//                     right: ExprInteger { value: 4 }.into(),
//                     left: Box::from(ExprBinary {
//                         kind: ExprKind::Plus,
//                         left: ExprInteger { value: 1 }.into(),
//                         right: Box::from(ExprBinary {
//                             kind: ExprKind::Plus,
//                             left: Box::from(ExprInteger { value: 2 }),
//                             right: Box::from(ExprInteger { value: 3 }),
//                         }),
//                     }),
//
//                 }
//             }
//         );
//
//         assert_eq!(
//             parser.next_statement(),
//             StmtExpr {
//                 value: ExprBinary {
//                     kind: ExprKind::Times,
//                     right: ExprInteger { value: 2 }.into(),
//                     left: Box::from(ExprBinary {
//                         kind: ExprKind::Plus,
//                         left: ExprInteger { value: 5 }.into(),
//                         right: ExprInteger { value: 5 }.into(),
//                     }),
//
//                 }
//             }
//         );
//
//         assert_eq!(
//             parser.next_statement(),
//             StmtExpr {
//                 value: ExprBinary {
//                     kind: ExprKind::Div,
//                     left: ExprInteger { value: 2 }.into(),
//                     right: Box::from(ExprBinary {
//                         kind: ExprKind::Plus,
//                         left: ExprInteger { value: 5 }.into(),
//                         right: ExprInteger { value: 5 }.into(),
//                     }),
//
//                 }
//             }
//         );
//
//         assert_eq!(
//             parser.next_statement(),
//             StmtExpr {
//                 value: ExprPrefix {
//                     kind: PrefixKind::Minus,
//                     value: Box::from(ExprBinary {
//                         kind: ExprKind::Plus,
//                         left: ExprInteger { value: 5 }.into(),
//                         right: ExprInteger { value: 5 }.into(),
//                     }),
//
//                 }
//             }
//         );
//
//         assert_eq!(
//             parser.next_statement(),
//             StmtExpr {
//                 value: ExprPrefix {
//                     kind: PrefixKind::Not,
//                     value: Box::from(ExprBinary {
//                         kind: ExprKind::Equals,
//                         left: ExprBool { value: true }.into(),
//                         right: ExprBool { value: true }.into(),
//                     }),
//                 }
//             }
//         );
//     }
// }
