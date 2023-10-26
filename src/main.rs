pub mod lexer;
pub mod token;

use crate::lexer::Lexer;
fn main() -> () {
    let input = String::from("let x = 5").bytes().collect();
    let mut lexer = Lexer::new(input);
    let lexed = lexer.lex();
    match lexed {
        Ok(l) => println!("{}", l),
        Err(errors) => {
            for e in errors {
                println!("{}", e);
            }
        }
    }
}
