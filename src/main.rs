pub mod ast;
pub mod lexer;
pub mod parser;
pub mod token;

use parser::Parser;
use token::Token;

use crate::{ast::Node, lexer::Lexer};
fn main() -> () {
    let input = String::from("let y = 5555555;").bytes().collect();
    let mut lexer = Lexer::new(input);
    let mut lexed = match lexer.lex() {
        Ok(l) => l,
        Err(errors) => {
            for e in errors {
                println!("{}", e);
            }
            vec![Token::new(
                token::TokenType::ILLEGAL,
                String::from(""),
                None,
                0,
                0,
            )]
        }
    };

    let mut parser = Parser::new(&mut lexed);
    let program = parser.parse_program();
    println!("{}", program.string());
}
