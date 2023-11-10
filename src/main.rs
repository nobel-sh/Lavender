pub mod ast;
pub mod environment;
pub mod evaluator;
pub mod lexer;
pub mod object;
pub mod parser;
pub mod token;

use environment::Environment;
use evaluator::Evaluator;
use parser::Parser;
use token::Token;

use crate::lexer::lexer::Lexer;
fn main() -> () {
    let input = String::from(
        // func
        //TODO: add string operations
        r#"
        let fib = func (x) {
            if (x == 0) {
                return 0;
            } else {
                if (x == 1) {
                    return 1;
                } else {
                    return fib(x - 1) + fib(x - 2);
                }
            }
        };
        fib(20)
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
            vec![Token::new(
                token::TokenType::ILLEGAL,
                String::from(""),
                None,
                0,
                0,
            )]
        }
    };
    let program = Parser::new(tokens).parse_program();
    println!("**********");
    let env = Environment::new();
    let result = Evaluator::new(env).eval(&program);
    println!("{:?}", result);
}
