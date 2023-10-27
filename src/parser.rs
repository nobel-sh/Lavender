use std::fmt::Debug;

use crate::ast::*;
use crate::token;
use crate::token::TokenType;

use token::Token;
pub struct Parser<'a> {
    tokens: &'a mut Vec<Token>,
    current_token: Token,
    peek_token: Token,
    errors: Vec<String>,
}

pub struct ParserError {
    pub message: String,
}
impl Debug for ParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:#?}", self.message)
    }
}

type parser_prefix_func = fn(&mut Parser) -> Result<Expression, ParserError>;
type parse_infix_func = fn(&mut Parser, Expression) -> Result<Expression, ParserError>;

#[derive(PartialOrd, PartialEq)]
pub enum Precedence {
    LOWEST,
    EQUALS,
    LESSGREATER,
    SUM,
    PRODUCT,
    PREFIX,
    CALL,
}

impl Precedence {
    pub fn from_token_type(token_type: token::TokenType) -> Precedence {
        match token_type {
            token::TokenType::LPAREN => Precedence::CALL,
            token::TokenType::SLASH => Precedence::PRODUCT,
            token::TokenType::ASTERISK => Precedence::PRODUCT,
            token::TokenType::MINUS => Precedence::SUM,
            token::TokenType::PLUS => Precedence::SUM,
            token::TokenType::LESS => Precedence::LESSGREATER,
            token::TokenType::GREATER => Precedence::LESSGREATER,
            token::TokenType::EQUAL => Precedence::EQUALS,
            token::TokenType::BANGEQUAL => Precedence::EQUALS,
            _ => Precedence::LOWEST,
        }
    }
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &mut Vec<Token>) -> Parser {
        let current_token = tokens[0].clone();
        let peek_token = tokens[1].clone();
        tokens.remove(0);
        tokens.remove(0);
        Parser {
            tokens,
            current_token,
            peek_token,
            errors: Vec::new(),
        }
    }

    pub fn parse_program(&mut self) -> Program {
        let mut program = Program {
            statements: Vec::new(),
        };

        while !self.current_token_is(token::TokenType::EOF) {
            let stmt = self.parse_statement();
            if let Some(stmt) = stmt {
                program.statements.push(stmt);
            }
            self.next_token();
        }

        program
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        match self.current_token.token_type {
            token::TokenType::LET => self.parse_let_statement(),
            token::TokenType::RETURN => self.parse_return_statement(),
            _ => None,
        }
    }

    fn parse_let_statement(&mut self) -> Option<Statement> {
        let token = self.current_token.clone();
        if !self.expect_peek(token::TokenType::IDENTIFIER) {
            return None;
        }
        let name = Identifier {
            token: self.current_token.clone(),
            value: self.current_token.lexeme.clone(),
        };
        if !self.expect_peek(token::TokenType::EQUAL) {
            return None;
        }
        self.next_token();
        // let value = self.parse_expression(Precedence::LOWEST).unwrap();
        while !self.peek_token_is(token::TokenType::SEMICOLON) {
            self.next_token();
        }
        Some(Statement::LetStatement(LetStatement {
            token,
            name,
            value: Box::new(Expression::Identifier(Identifier {
                token: self.current_token.clone(),
                value: self.current_token.lexeme.clone(),
            })),
        }))
    }

    fn parse_return_statement(&mut self) -> Option<Statement> {
        let token = self.current_token.clone();
        self.next_token();
        while !self.peek_token_is(token::TokenType::SEMICOLON) {
            self.next_token();
        }
        Some(Statement::ReturnStatement(ReturnStatement {
            token,
            return_value: Box::new(Expression::Identifier(Identifier {
                token: self.current_token.clone(),
                value: self.current_token.lexeme.clone(),
            })),
        }))
    }

    fn parse_expression_statement(&mut self) -> Option<Statement> {
        let token = self.current_token.clone();
        let expression = Box::new(self.parse_expression(Precedence::LOWEST).unwrap());
        if self.peek_token_is(token::TokenType::SEMICOLON) {
            self.next_token();
        }
        Some(Statement::ExpressionStatement(ExpressionStatement {
            token,
            expression,
        }))
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Option<Expression> {
        let prefix = self.parse_prefix();
        if prefix.is_none() {
            return None;
        }
        let mut left_exp = prefix.unwrap()(self);
        while !self.peek_token_is(token::TokenType::SEMICOLON)
            && precedence < Precedence::from_token_type(self.peek_token.token_type)
        {
            let infix = self.parse_infix();
            if infix.is_none() {
                return Some(left_exp);
            }
            self.next_token();
            left_exp = infix.unwrap()(self, left_exp.unwrap());
        }
        Some(left_exp.unwrap())
    }

    fn expect_peek(&mut self, token_type: token::TokenType) -> bool {
        if self.peek_token_is(token_type) {
            self.next_token();
            true
        } else {
            self.peek_error(token_type);
            false
        }
    }

    fn peek_token_is(&self, token_type: token::TokenType) -> bool {
        self.peek_token.token_type == token_type
    }

    fn peek_error(&mut self, token_type: token::TokenType) {
        let msg = format!(
            "expected next token to be {:?}, got {:?} instead",
            token_type, self.peek_token.token_type
        );
        self.errors.push(msg);
    }

    fn current_token_is(&self, token_type: token::TokenType) -> bool {
        self.current_token.token_type == token_type
    }

    fn next_token(&mut self) {
        self.current_token = self.peek_token.clone();
        if self.current_token_is(token::TokenType::EOF) {
            return;
        }
        self.peek_token = self.tokens.remove(0);
    }

    fn parse_prefix(&self) -> Option<parser_prefix_func> {
        Some(match &self.current_token.token_type {
            TokenType::IDENTIFIER => Parser::parse_identifier_expression,
            TokenType::INT => Parser::parse_integer_literal,
            TokenType::STRING => Parser::parse_string_literal,
            TokenType::BOOL => Parser::parse_boolean_literal,
            TokenType::BANG | TokenType::MINUS => Parser::parse_prefix_literal,
            TokenType::LPAREN => Parser::parse_grouped_expression,
            TokenType::IF => Parser::parse_if_expression,
            TokenType::FUNC => Parser::parse_function_literal,
            _ => return None,
        })
    }

    fn parse_infix(&self) -> Option<parse_infix_func> {
        Some(match &self.current_token.token_type {
            TokenType::PLUS
            | TokenType::MINUS
            | TokenType::SLASH
            | TokenType::ASTERISK
            | TokenType::EQUAL
            | TokenType::BANGEQUAL
            | TokenType::LESS
            | TokenType::GREATER => Parser::parse_infix_expression,
            TokenType::LPAREN => Parser::parse_call_expression,
            _ => return None,
        })
    }

    fn parse_identifier_expression(&mut self) -> Result<Expression, ParserError> {
        Ok(Expression::Identifier(Identifier {
            token: self.current_token.clone(),
            value: self.current_token.lexeme.clone(),
        }))
    }

    fn parse_integer_literal(&mut self) -> Result<Expression, ParserError> {
        let value = self.current_token.lexeme.parse::<i64>().unwrap();
        Ok(Expression::Literal(Literal::Integer(value)))
    }

    fn parse_string_literal(&mut self) -> Result<Expression, ParserError> {
        Ok(Expression::Literal(Literal::String(
            self.current_token.lexeme.clone(),
        )))
    }

    fn parse_boolean_literal(&mut self) -> Result<Expression, ParserError> {
        let value = self.current_token.lexeme.parse::<bool>().unwrap();
        Ok(Expression::Literal(Literal::Boolean(value)))
    }

    fn parse_grouped_expression(&mut self) -> Result<Expression, ParserError> {
        self.next_token();
        let exp = self.parse_expression(Precedence::LOWEST).unwrap();
        if !self.expect_peek(token::TokenType::RPAREN) {
            return Err(ParserError {
                message: String::from("expected )"),
            });
        }
        Ok(exp)
    }

    fn parse_if_expression(&mut self) -> Result<Expression, ParserError> {
        todo!("parse if expressions")
    }

    fn parse_function_literal(&mut self) -> Result<Expression, ParserError> {
        todo!("parse fuctions")
    }

    fn parse_expression_call(&mut self, function: Expression) -> Result<Expression, ParserError> {
        todo!("parse function calls")
    }

    fn parse_expression_array(&mut self) -> Result<Expression, ParserError> {
        todo!("parse_expression_array")
    }

    fn parse_prefix_literal(&mut self) -> Result<Expression, ParserError> {
        todo!("parse_expression_prefix")
    }

    fn parse_infix_expression(&mut self, left: Expression) -> Result<Expression, ParserError> {
        todo!("parse_expression_infix")
    }

    fn parse_call_expression(&mut self, function: Expression) -> Result<Expression, ParserError> {
        todo!("parse_call_expression")
    }
}
