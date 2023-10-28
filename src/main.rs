pub mod ast;
pub mod lexer;
pub mod parser;
pub mod token;

use parser::Parser;
use token::Token;

use crate::lexer::lexer::Lexer;
fn main() -> () {
    let input = String::from(
        "
    if (5 < 10) {
        return true;
    } else {
        return false;
    }
    ",
    )
    .bytes()
    .collect();
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

    let mut parser = Parser::new(lexed);
    let program = parser.parse_program();
    println!("{}", program);
}
