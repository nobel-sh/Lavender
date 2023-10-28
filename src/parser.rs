use std::fmt::Debug;

use crate::ast::*;
use crate::token;
use crate::token::TokenType;

use token::Token;
pub struct Parser {
    tokens: Vec<Token>,
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

type ParserPrefixFunc = fn(&mut Parser) -> Result<Expression, ParserError>;
type ParserInfixFunc = fn(&mut Parser, Expression) -> Result<Expression, ParserError>;

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

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Parser {
        let current_token = tokens[0].clone();
        let peek_token = tokens[1].clone();

        let mut parser = Parser {
            tokens,
            current_token,
            peek_token,
            errors: Vec::new(),
        };
        parser.tokens.remove(0);
        parser.tokens.remove(0);
        parser
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
        println!("parse_statement: {:?}", self.current_token.token_type);
        match self.current_token.token_type {
            token::TokenType::LET => self.parse_let_statement(),
            token::TokenType::RETURN => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
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
        let value = self.parse_expression(Precedence::LOWEST).unwrap();
        if self.peek_token_is(token::TokenType::SEMICOLON) {
            self.next_token();
        }
        Some(Statement::LetStatement(LetStatement {
            token,
            name,
            value: Box::new(value),
        }))
    }

    fn parse_return_statement(&mut self) -> Option<Statement> {
        let token = self.current_token.clone();
        self.next_token();
        let return_value = self.parse_expression(Precedence::LOWEST).unwrap();
        if self.peek_token_is(token::TokenType::SEMICOLON) {
            self.next_token();
        }
        Some(Statement::ReturnStatement(ReturnStatement {
            token,
            return_value: Box::new(return_value),
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

    fn parse_expression(&mut self, precedence: Precedence) -> Result<Expression, ParserError> {
        let prefix = self.parse_prefix().ok_or_else(|| ParserError {
            message: String::from("no prefix parse function found"),
        });
        let mut left_exp = prefix.unwrap()(self);
        while !self.peek_token_is(token::TokenType::SEMICOLON)
            && precedence < Precedence::from_token_type(self.peek_token.token_type)
        {
            let infix = self.parse_infix();
            if infix.is_none() {
                return left_exp;
            }
            self.next_token();
            left_exp = infix.unwrap()(self, left_exp.unwrap());
        }
        left_exp
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

    fn parse_prefix(&self) -> Option<ParserPrefixFunc> {
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

    fn parse_infix(&self) -> Option<ParserInfixFunc> {
        Some(match &self.peek_token.token_type {
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
        let exp = Expression::Identifier(Identifier {
            token: self.current_token.clone(),
            value: self.current_token.lexeme.clone(),
        });
        Ok(exp)
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

    fn parse_block_statement(&mut self) -> Result<Statement, ParserError> {
        let mut statements = Vec::new();
        self.next_token();
        while !self.current_token_is(token::TokenType::RBRACE)
            && !self.current_token_is(token::TokenType::EOF)
        {
            let stmt = self.parse_statement();
            if let Some(stmt) = stmt {
                statements.push(stmt);
            }
            self.next_token();
        }
        Ok(Statement::BlockStatement(BlockStatement {
            token: self.current_token.clone(),
            statements,
        }))
    }

    fn parse_if_expression(&mut self) -> Result<Expression, ParserError> {
        self.next_token();
        if !self.expect_peek(token::TokenType::LPAREN) {
            return Err(ParserError {
                message: String::from("expected ("),
            });
        }
        self.next_token();
        let condition = self.parse_expression(Precedence::LOWEST).unwrap();
        if !self.expect_peek(token::TokenType::RPAREN) {
            return Err(ParserError {
                message: String::from("expected )"),
            });
        }
        if !self.expect_peek(token::TokenType::LBRACE) {
            return Err(ParserError {
                message: String::from("expected {"),
            });
        }
        let consequence = self.parse_block_statement().unwrap();
        let mut alternative = None;
        if self.peek_token_is(token::TokenType::ELSE) {
            self.next_token();
            if !self.expect_peek(token::TokenType::LBRACE) {
                return Err(ParserError {
                    message: String::from("expected {"),
                });
            }
            alternative = Some(Box::new(self.parse_block_statement().unwrap()));
        }
        Ok(Expression::IfExpression(IfExpression {
            token: self.current_token.clone(),
            condition: Box::new(condition),
            consequence: Box::new(consequence),
            alternative,
        }))
    }

    fn parse_function_literal(&mut self) -> Result<Expression, ParserError> {
        let token = self.current_token.clone();
        if !self.expect_peek(token::TokenType::LPAREN) {
            return Err(ParserError {
                message: String::from("expected ("),
            });
        }
        let parameters = self.parse_function_parameters().unwrap();
        if !self.expect_peek(token::TokenType::LBRACE) {
            return Err(ParserError {
                message: String::from("expected {"),
            });
        }
        let body = self.parse_block_statement().unwrap();
        Ok(Expression::FunctionExpression(FunctionExpression {
            token,
            parameters,
            body: Box::new(body),
        }))
    }

    fn parse_function_parameters(&mut self) -> Result<Vec<Identifier>, ParserError> {
        let mut identifiers = Vec::new();
        if self.peek_token_is(token::TokenType::RPAREN) {
            self.next_token();
            return Ok(identifiers);
        }
        self.next_token();
        let ident = Identifier {
            token: self.current_token.clone(),
            value: self.current_token.lexeme.clone(),
        };
        identifiers.push(ident);
        while self.peek_token_is(token::TokenType::COMMA) {
            self.next_token();
            self.next_token();
            let ident = Identifier {
                token: self.current_token.clone(),
                value: self.current_token.lexeme.clone(),
            };
            identifiers.push(ident);
        }
        if !self.expect_peek(token::TokenType::RPAREN) {
            return Err(ParserError {
                message: String::from("expected )"),
            });
        }
        Ok(identifiers)
    }

    fn parse_prefix_literal(&mut self) -> Result<Expression, ParserError> {
        let token = self.current_token.clone();
        let operator = self.current_token.lexeme.clone();
        self.next_token();
        let right = self.parse_expression(Precedence::PREFIX).unwrap();
        Ok(Expression::PrefixExpression(PrefixExpression {
            token,
            operator,
            right: Box::new(right),
        }))
    }

    fn parse_infix_expression(&mut self, left: Expression) -> Result<Expression, ParserError> {
        let token = self.current_token.clone();
        let operator = self.current_token.lexeme.clone();
        let precedence = Precedence::from_token_type(self.current_token.token_type);
        self.next_token();
        let right = self.parse_expression(precedence).unwrap();
        Ok(Expression::InfixExpression(InfixExpression {
            token,
            operator,
            left: Box::new(left),
            right: Box::new(right),
        }))
    }

    fn parse_call_expression(&mut self, function: Expression) -> Result<Expression, ParserError> {
        let token = self.current_token.clone();
        let arguments = self.parse_call_arguments().unwrap();
        Ok(Expression::FunctionCallExpression(FunctionCallExpression {
            token,
            arguments,
            function: Box::new(function),
        }))
    }

    fn parse_call_arguments(&mut self) -> Result<Vec<Expression>, ParserError> {
        let mut arguments = Vec::new();
        if self.peek_token_is(token::TokenType::RPAREN) {
            self.next_token();
            return Ok(arguments);
        }
        self.next_token();
        arguments.push(self.parse_expression(Precedence::LOWEST).unwrap());
        while self.peek_token_is(token::TokenType::COMMA) {
            self.next_token();
            self.next_token();
            arguments.push(self.parse_expression(Precedence::LOWEST).unwrap());
        }
        if !self.expect_peek(token::TokenType::RPAREN) {
            return Err(ParserError {
                message: String::from("expected )"),
            });
        }
        Ok(arguments)
    }
}