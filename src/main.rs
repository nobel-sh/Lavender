pub mod ast;
pub mod environment;
pub mod evaluator;
pub mod lexer;
pub mod object;
pub mod parser;
pub mod token;

use parser::Parser;
use token::Token;

use crate::lexer::lexer::Lexer;
fn main() -> () {
    let input = String::from(
        r#"
        let add = func (x,y) {
            return x+y;
        };
        add(1,2);
    "#,
    )
    .bytes()
    .collect();
    let mut lexer = Lexer::new(input);
    let lexed = match lexer.lex() {
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
    // println!("{}", program);
    let mut env = environment::Environment::new();
    // let result = evaluator::eval_program(&program, &mut env).unwrap();
    println!("**********");
    match evaluator::eval_program(&program, &mut env) {
        Some(r) => println!("{}", r),
        None => println!("None"),
    }
    // println!("{:?}", result);
}
