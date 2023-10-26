use std::fmt::{self, Display};

use crate::token::{Literal, Token, TokenType, KEYWORDS_MAP};

pub struct Lexer {
    input: Vec<u8>,
    tokens: Vec<Token>,
    start: usize,
    current: usize,
    line: usize,
    column: usize,
    errors: Vec<LexerError>,
}

impl Lexer {
    pub fn new(input: Vec<u8>) -> Self {
        Lexer {
            input,
            tokens: Vec::new(),
            start: 0,
            current: 0,
            line: 1,
            column: 0,
            errors: Vec::new(),
        }
    }
    pub fn lex(&mut self) -> Result<&Lexer, Vec<LexerError>> {
        while !self.is_at_end() {
            self.start = self.current;
            self.scan_token();
        }

        if self.errors.is_empty() {
            Ok(self)
        } else {
            Err(std::mem::take(&mut self.errors))
        }
    }
    fn scan_token(&mut self) {
        let c = self.advance();
        match c {
            b'(' => self.add_token(TokenType::LPAREN, None),
            b')' => self.add_token(TokenType::RPAREN, None),
            b'{' => self.add_token(TokenType::LBRACE, None),
            b'}' => self.add_token(TokenType::RBRACE, None),
            b',' => self.add_token(TokenType::COMMA, None),
            b'.' => self.add_token(TokenType::DOT, None),
            b'-' => self.add_token(TokenType::MINUS, None),
            b'+' => self.add_token(TokenType::PLUS, None),
            b';' => self.add_token(TokenType::SEMICOLON, None),
            b'*' => self.add_token(TokenType::ASTERISK, None),
            b'\n' => {
                self.line += 1;
                self.column = 1;
            }
            b'/' => {
                if self.match_char(b'/') {
                    while self.peek_char() != b'\n' && !self.is_at_end() {
                        self.advance();
                    }
                } else {
                    self.add_token(TokenType::SLASH, None);
                }
            }
            b'=' => {
                let token_type = if self.match_char(b'=') {
                    TokenType::EQUALEQUAL
                } else {
                    TokenType::EQUAL
                };
                self.add_token(token_type, None);
            }
            b'<' => {
                let token_type = if self.match_char(b'=') {
                    TokenType::LESSEQUAL
                } else {
                    TokenType::LESS
                };
                self.add_token(token_type, None);
            }
            b'>' => {
                let token_type = if self.match_char(b'=') {
                    TokenType::GREATEREQUAL
                } else {
                    TokenType::GREATER
                };
                self.add_token(token_type, None);
            }
            b'!' => {
                let token_type = if self.match_char(b'=') {
                    TokenType::BANGEQUAL
                } else {
                    TokenType::BANG
                };
                self.add_token(token_type, None);
            }
            b'"' => self.string(),
            b' ' | b'\r' | b'\t' => {}
            _ => {
                if c.is_ascii_digit() {
                    self.number();
                } else if c.is_ascii_alphabetic() {
                    self.identifier();
                } else {
                    self.errors.push(LexerError::new(
                        self.line,
                        self.column,
                        format!("Unexpected character: {}", c as char),
                    ));
                    self.add_token(TokenType::ILLEGAL, None);
                }
            }
        }
    }
    fn string(&mut self) {
        while self.peek_char() != b'"' && !self.is_at_end() {
            if self.peek_char() == b'\n' {
                self.column = 1;
                self.line += 1;
            }
            self.advance();
        }
        if self.is_at_end() {
            panic!("Unterminated string");
        }
        self.advance();
        let value = self.input[self.start + 1..self.current - 1].to_vec();
        let string_literal = String::from_utf8(value).unwrap();

        self.add_token(TokenType::STRING, Some(Literal::STRING(string_literal)));
    }

    fn number(&mut self) {
        let mut token_type = TokenType::INT;
        while self.peek_char().is_ascii_digit() {
            self.advance();
        }
        if self.peek_char() == b'.' && self.peek_next_char().is_ascii_digit() {
            token_type = TokenType::FLOAT;
            self.advance();
            while self.peek_char().is_ascii_digit() {
                self.advance();
            }
        }
        let value = self.input[self.start..self.current].to_vec();
        let num_literal = String::from_utf8(value).unwrap();
        match token_type {
            TokenType::INT => {
                let num = num_literal.parse::<i64>().unwrap();
                self.add_token(token_type, Some(Literal::INT(num)));
            }
            TokenType::FLOAT => {
                let num = num_literal.parse::<f64>().unwrap();
                self.add_token(token_type, Some(Literal::FLOAT(num)));
            }
            _ => {
                self.add_token(TokenType::ILLEGAL, None);
                self.errors.push(LexerError::new(
                    self.line,
                    self.column,
                    format!("Unexpected number: {}", num_literal),
                ));
            }
        }
    }
    fn identifier(&mut self) {
        while self.peek_char().is_ascii_alphanumeric() {
            self.advance();
        }
        let value = self.input[self.start..self.current].to_vec();
        let identifier_literal = String::from_utf8(value).unwrap();
        let token_type = KEYWORDS_MAP
            .get(&identifier_literal.as_str())
            .map(|&x| x)
            .unwrap_or(TokenType::IDENTIFIER);
        match token_type {
            TokenType::IDENTIFIER => {
                self.add_token(token_type, Some(Literal::IDENTIFIER(identifier_literal)));
            }
            _ => self.add_token(token_type, None),
        }
    }

    fn advance(&mut self) -> u8 {
        self.column += 1;
        self.current += 1;
        self.input[self.current - 1]
    }
    fn is_at_end(&mut self) -> bool {
        self.current >= self.input.len()
    }

    fn match_char(&mut self, expected: u8) -> bool {
        if self.is_at_end() {
            return false;
        }
        if self.input[self.current] != expected {
            return false;
        }
        self.current += 1;
        true
    }
    fn peek_char(&mut self) -> u8 {
        if self.is_at_end() {
            return b'\0';
        }
        self.input[self.current]
    }
    fn peek_next_char(&mut self) -> u8 {
        if self.current + 1 >= self.input.len() {
            return b'\0';
        }
        self.input[self.current + 1]
    }
    fn add_token(&mut self, token_type: TokenType, literal: Option<Literal>) {
        let text = self.input[self.start..self.current].to_vec();
        let lexeme = String::from_utf8(text).unwrap();
        self.tokens.push(Token::new(
            token_type,
            lexeme,
            literal,
            self.line,
            self.column,
        ));
    }
}

impl Display for Lexer {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Line: {}", self.line)?;
        writeln!(f, "Tokens: ")?;
        for token in &self.tokens {
            writeln!(f, "{}", token)?;
        }
        Ok(())
    }
}

impl std::fmt::Debug for Lexer {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Line: {}", self.line)?;
        writeln!(f, "Tokens: ")?;
        for token in &self.tokens {
            writeln!(f, "{}", token)?;
        }
        Ok(())
    }
}

#[derive(Debug)]
pub struct LexerError {
    line: usize,
    column: usize,
    message: String,
}

impl LexerError {
    pub fn new(line: usize, column: usize, message: String) -> Self {
        LexerError {
            line,
            column,
            message,
        }
    }
}

impl Display for LexerError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(
            f,
            "{} at line: {}, column: {}",
            self.message, self.line, self.column
        )
    }
}
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lexer() {
        let input = "int x = 5;";
        let mut lexer = Lexer::new(input.bytes().collect());
        lexer.lex().unwrap();

        assert_eq!(lexer.tokens.len(), 5);
        assert_eq!(lexer.tokens[0].token_type, TokenType::INT);
        assert_eq!(lexer.tokens[0].lexeme, "int");
        assert_eq!(lexer.tokens[1].token_type, TokenType::IDENTIFIER);
        assert_eq!(lexer.tokens[1].lexeme, "x");
        assert_eq!(lexer.tokens[2].token_type, TokenType::EQUAL);
        assert_eq!(lexer.tokens[2].lexeme, "=");
        assert_eq!(lexer.tokens[3].token_type, TokenType::INT);
        assert_eq!(lexer.tokens[3].lexeme, "5");
        assert_eq!(lexer.tokens[4].token_type, TokenType::SEMICOLON);
        assert_eq!(lexer.tokens[4].lexeme, ";");
    }

    #[test]
    fn test_lexer_error() {
        let input = "let x = 5 %";
        let mut lexer = Lexer::new(input.bytes().collect());
        let errors = lexer.lex().unwrap_err();
        assert_eq!(errors.len(), 1);
        assert_eq!(errors[0].message, "Unexpected character: %");
        assert_eq!(errors[0].line, 1);
        assert_eq!(errors[0].column, 11);
    }
}
