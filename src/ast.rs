use std::fmt::Display;

use crate::token::Token;

#[derive(Debug)]
pub enum Node {
    Program(Program),
    Statement(Statement),
    Expression(Expression),
}

#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Statement>,
}

impl Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut result = String::new();
        println!("Program: {:#?}", self.statements);
        for s in &self.statements {
            result.push_str(&s.to_string());
        }
        write!(f, "{}", result)
    }
}

#[derive(Debug, Clone)]
pub enum Statement {
    LetStatement(LetStatement),
    ReturnStatement(ReturnStatement),
    ExpressionStatement(ExpressionStatement),
    BlockStatement(BlockStatement),
}

impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Statement::LetStatement(l) => write!(f, "{}", l),
            Statement::ReturnStatement(r) => write!(f, "{}", r),
            Statement::ExpressionStatement(e) => write!(f, "{}", e),
            Statement::BlockStatement(b) => write!(f, "{}", b),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Expression {
    Identifier(Identifier),
    Literal(Literal),
    IfExpression(IfExpression),
    PrefixExpression(PrefixExpression),
    InfixExpression(InfixExpression),
    FunctionExpression(FunctionExpression),
    FunctionCallExpression(FunctionCallExpression),
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Identifier(i) => write!(f, "{}", i.value),
            Expression::Literal(l) => write!(f, "{}", l),
            Expression::IfExpression(i) => write!(f, "{}", i),
            Expression::PrefixExpression(p) => write!(f, "{}", p),
            Expression::InfixExpression(i) => write!(f, "{}", i),
            Expression::FunctionExpression(func) => write!(f, "{}", func),
            Expression::FunctionCallExpression(call) => write!(f, "{}", call),
        }
    }
}

#[derive(Debug, Clone)]
pub struct IfExpression {
    pub token: Token,
    pub condition: Box<Expression>,
    pub consequence: Box<Statement>,
    pub alternative: Option<Box<Statement>>,
}

impl Display for IfExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut result = String::from("if");
        result.push_str(&self.condition.to_string());
        result.push_str(" ");
        result.push_str(&self.consequence.to_string());
        if let Some(alt) = &self.alternative {
            result.push_str("else ");
            result.push_str(&alt.to_string());
        }
        write!(f, "{}", result)
    }
}

#[derive(Debug, Clone)]
pub struct PrefixExpression {
    pub token: Token,
    pub operator: String,
    pub right: Box<Expression>,
}

impl Display for PrefixExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}", self.operator, self.right)
    }
}

#[derive(Debug, Clone)]
pub struct InfixExpression {
    pub token: Token,
    pub left: Box<Expression>,
    pub operator: String,
    pub right: Box<Expression>,
}

impl Display for InfixExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {} {}", self.left, self.operator, self.right)
    }
}

#[derive(Debug, Clone)]
pub enum Literal {
    Integer(i64),
    Boolean(bool),
    String(String),
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::Integer(i) => write!(f, "{}", i),
            Literal::Boolean(b) => write!(f, "{}", b),
            Literal::String(s) => write!(f, "{}", s),
        }
    }
}

#[derive(Debug, Clone)]
pub struct LetStatement {
    pub token: Token,
    pub name: Identifier,
    pub value: Box<Expression>,
}

impl Display for LetStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} {} = {};",
            self.token.lexeme, self.name.value, self.value
        )
    }
}

#[derive(Debug, Clone)]
pub struct ReturnStatement {
    pub token: Token,
    pub return_value: Box<Expression>,
}

impl Display for ReturnStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {};", self.token.lexeme, self.return_value)
    }
}

#[derive(Debug, Clone)]
pub struct ExpressionStatement {
    pub token: Token,
    pub expression: Box<Expression>,
}

impl Display for ExpressionStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{};", self.expression)
    }
}

#[derive(Debug, Clone)]
pub struct BlockStatement {
    pub token: Token,
    pub statements: Vec<Statement>,
}

impl Display for BlockStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut result = String::from("{");
        for s in &self.statements {
            result.push_str(&s.to_string());
        }
        result.push_str("}");
        write!(f, "{}", result)
    }
}

#[derive(Debug, Clone)]
pub struct Identifier {
    pub token: Token,
    pub value: String,
}

impl Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug, Clone)]
pub struct FunctionExpression {
    pub token: Token,
    pub parameters: Vec<Identifier>,
    pub body: Box<Statement>,
}

impl Display for FunctionExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut result = String::from("fn(");
        for (i, p) in self.parameters.iter().enumerate() {
            result.push_str(&p.to_string());
            if i < self.parameters.len() - 1 {
                result.push_str(", ");
            }
        }
        result.push_str(") ");
        result.push_str(&self.body.to_string());
        write!(f, "{}", result)
    }
}

#[derive(Debug, Clone)]
pub struct FunctionCallExpression {
    pub token: Token,
    pub function: Box<Expression>,
    pub arguments: Vec<Expression>,
}

impl Display for FunctionCallExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut result = String::from("(");
        for (i, a) in self.arguments.iter().enumerate() {
            result.push_str(&a.to_string());
            if i < self.arguments.len() - 1 {
                result.push_str(", ");
            }
        }
        result.push_str(")");
        write!(f, "{}{}", self.function, result)
    }
}
