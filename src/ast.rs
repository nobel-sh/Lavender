use std::fmt::Display;

use crate::token;

pub trait Node {
    fn token_literal(&self) -> String;
    fn string(&self) -> String {
        String::from("Not implemented")
    }
}

pub trait Statement: Node {
    fn node_statement(&self);
}

pub trait Expression: Node {
    fn expression_statement(&self);
}

pub struct Program {
    pub statements: Vec<Box<dyn Statement>>,
}

impl Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut out = String::new();
        for s in &self.statements {
            out.push_str(&s.string());
        }
        write!(f, "{}", out)
    }
}

impl Node for Program {
    fn token_literal(&self) -> String {
        if self.statements.len() > 0 {
            self.statements[0].token_literal()
        } else {
            String::from("")
        }
    }

    fn string(&self) -> String {
        let mut out = String::new();
        for s in &self.statements {
            out.push_str(&s.string());
        }
        out
    }
}

pub struct LetStatement {
    pub token: token::Token,
    pub name: Identifier,
    pub value: Box<dyn Expression>,
}

impl Node for LetStatement {
    fn token_literal(&self) -> String {
        self.token.lexeme.clone()
    }

    fn string(&self) -> String {
        let mut out = String::new();
        out.push_str(&self.token_literal());
        out.push_str(" ");
        out.push_str(&self.name.string());
        out.push_str(" = ");
        out.push_str(self.value.string().to_string().as_str());
        out.push_str(";");
        out
    }
}

impl Statement for LetStatement {
    fn node_statement(&self) {}
}

pub struct Identifier {
    pub token: token::Token,
    pub value: String,
}

impl Node for Identifier {
    fn token_literal(&self) -> String {
        self.token.lexeme.clone()
    }

    fn string(&self) -> String {
        self.value.clone()
    }
}

impl Expression for Identifier {
    fn expression_statement(&self) {}
}

pub struct ReturnStatement {
    pub token: token::Token,
    pub return_value: Box<dyn Expression>,
}

impl Node for ReturnStatement {
    fn token_literal(&self) -> String {
        self.token.lexeme.clone()
    }

    fn string(&self) -> String {
        let mut out = String::new();
        out.push_str(&self.token_literal());
        out.push_str(" ");
        out.push_str(self.return_value.string().to_string().as_str());
        out.push_str(";");
        out
    }
}

impl Statement for ReturnStatement {
    fn node_statement(&self) {}
}

pub struct ExpressionStatement {
    pub token: token::Token,
    pub expression: Box<dyn Expression>,
}

impl Node for ExpressionStatement {
    fn token_literal(&self) -> String {
        self.token.lexeme.clone()
    }

    fn string(&self) -> String {
        self.expression.string()
    }
}

impl Statement for ExpressionStatement {
    fn node_statement(&self) {}
}

pub struct IntegerLiteral {
    pub token: token::Token,
    pub value: i64,
}

impl Node for IntegerLiteral {
    fn token_literal(&self) -> String {
        self.token.lexeme.clone()
    }

    fn string(&self) -> String {
        self.token.lexeme.clone()
    }
}

impl Expression for IntegerLiteral {
    fn expression_statement(&self) {}
}
