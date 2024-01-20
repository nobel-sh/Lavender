pub mod ast;
pub mod environment;
pub mod evaluator;
pub mod lexer;
pub mod object;
pub mod parser;

use std::process::exit;

use environment::Environment;
use evaluator::Evaluator;
use parser::Parser;
// use token::Token;

use crate::lexer::lexer::Lexer;
fn main() -> () {
    let input = String::from(
        r#"
        let x = 5;
        while (x > 3) do
            x = x - 1;
        end
        x
"#,
    )
    .bytes()
    .collect();
    let tokens = match Lexer::new(input).lex() {
        Ok(l) => l,
        Err(errors) => {
            for e in errors {
                println!("{}", e);
            }
            exit(1);
        }
    };
    let program = Parser::new(tokens).parse_program();
    let env = Environment::new();
    let result = Evaluator::new(env).eval(&program);
    println!("{}", result.unwrap());
}
