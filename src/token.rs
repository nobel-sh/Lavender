use lazy_static::lazy_static;
use std::collections::HashMap;

#[derive(Hash, Debug, PartialEq, Eq, Clone, Copy)]
pub enum TokenType {
    // Single-character tokens.
    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,
    COMMA,
    DOT,
    PLUS,
    MINUS,
    ASTERISK,
    SLASH,
    SEMICOLON,

    // Single or Multi character tokens.
    BANG,
    BANGEQUAL,
    EQUAL,
    EQUALEQUAL,
    GREATER,
    GREATEREQUAL,
    LESS,
    LESSEQUAL,

    // Literals.
    IDENTIFIER,
    STRING,
    INT,
    FLOAT,
    BOOL,
    CHAR,

    // Keywords.
    IF,
    ELSE,
    TRUE,
    FALSE,
    WHILE,
    FUNC,
    NULL,
    AND,
    OR,
    LET,
    RETURN,

    // End of file.
    EOF,

    // Illegal token.
    ILLEGAL,
}

pub struct Token {
    pub token_type: TokenType,
    pub lexeme: String,
    pub literal: Option<Literal>,
    pub line: usize,
    pub column: usize,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Literal {
    INT(i64),
    FLOAT(f64),
    BOOL(bool),
    CHAR(char),
    STRING(String),
    IDENTIFIER(String),
}

impl Token {
    pub fn new(
        token_type: TokenType,
        lexeme: String,
        literal: Option<Literal>,
        line: usize,
        column: usize,
    ) -> Token {
        let lexeme_len = lexeme.len();
        Token {
            token_type,
            lexeme,
            literal,
            line,
            column: column - lexeme_len + 1,
        }
    }
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self.literal {
            Some(ref literal) => write!(
                f,
                "Token: {}\tLine: {}\tCol: {}  \tType: {:?}\tLiteral: {:?}",
                self.lexeme, self.line, self.column, self.token_type, literal
            ),
            None => write!(
                f,
                "Token: {}\tLine: {}\tCol: {}\tType: {:?}",
                self.lexeme, self.line, self.column, self.token_type
            ),
        }
    }
}

lazy_static! {
    pub static ref KEYWORDS_MAP: HashMap<&'static str, TokenType> = {
        let mut m = HashMap::new();
        m.insert("string", TokenType::STRING);
        m.insert("int", TokenType::INT);
        m.insert("float", TokenType::FLOAT);
        m.insert("bool", TokenType::BOOL);
        m.insert("char", TokenType::CHAR);
        m.insert("if", TokenType::IF);
        m.insert("else", TokenType::ELSE);
        m.insert("true", TokenType::TRUE);
        m.insert("false", TokenType::FALSE);
        m.insert("while", TokenType::WHILE);
        m.insert("func", TokenType::FUNC);
        m.insert("null", TokenType::NULL);
        m.insert("and", TokenType::AND);
        m.insert("or", TokenType::OR);
        m.insert("return", TokenType::RETURN);
        m.insert("let", TokenType::LET);
        m
    };
}
