use crate::ast::Expression;
use crate::ast::*;
use crate::environment::Environment;
use crate::object::Literal::{Boolean, Integer, String};
use crate::object::{FunctionObject, Object};
use std::cell::RefCell;
use std::rc::Rc; //Direct import of enum variants

pub struct Evaluator {
    env: Rc<RefCell<Environment>>,
}

type RustString = std::string::String;
pub struct EvaluatorError {
    pub message: RustString,
}

impl EvaluatorError {
    pub fn new(message: RustString) -> Self {
        EvaluatorError { message }
    }
}

impl std::fmt::Display for EvaluatorError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message.clone())
    }
}

impl Evaluator {
    pub fn new(new_env: Environment) -> Self {
        Evaluator {
            env: Rc::new(RefCell::new(new_env)),
        }
    }

    pub fn eval(&mut self, program: &Program) -> Option<Object> {
        let mut result = None;
        for statement in &program.statements {
            result = self.eval_statement(statement);
            match result {
                Some(Object::Return(_)) => return result,
                Some(_) => {}
                None => {}
            }
        }
        result
    }

    fn eval_statement(&mut self, statement: &Statement) -> Option<Object> {
        match statement {
            Statement::LetStatement(let_statement) => self.eval_let_statement(let_statement),
            Statement::ReturnStatement(return_statement) => {
                self.eval_return_statement(return_statement)
            }
            Statement::ExpressionStatement(expression_statement) => {
                self.eval_expression_statement(expression_statement)
            }
            Statement::BlockStatement(block_statemet) => self.eval_block_statement(block_statemet),
        }
    }

    fn eval_let_statement(&mut self, let_statement: &LetStatement) -> Option<Object> {
        let val = self.eval_expression(&let_statement.value);
        match val {
            Some(v) => self
                .env
                .borrow_mut()
                .set(let_statement.name.value.clone(), &v),
            None => Some(Object::Null),
        }
    }

    fn eval_return_statement(&mut self, return_statement: &ReturnStatement) -> Option<Object> {
        let val = self.eval_expression(&return_statement.return_value);
        match val {
            Some(v) => Some(Object::Return(Box::new(v))),
            None => Some(Object::Null),
        }
    }

    fn eval_expression_statement(
        &mut self,
        expression_statement: &ExpressionStatement,
    ) -> Option<Object> {
        self.eval_expression(&expression_statement.expression)
    }

    fn eval_block_statement(&mut self, block_statement: &BlockStatement) -> Option<Object> {
        let mut result = None;
        for statement in &block_statement.statements {
            result = self.eval_statement(statement);
            match result {
                Some(Object::Return(_)) => return result,
                Some(_) => {}
                None => {}
            }
        }
        result
    }

    fn eval_expression(&mut self, expression: &Expression) -> Option<Object> {
        match expression {
            Expression::Identifier(identifier) => self.eval_identifier(identifier),
            Expression::Literal(literal) => self.eval_literal(literal),
            Expression::PrefixExpression(prefix_expression) => {
                self.eval_prefix_expression(prefix_expression)
            }
            Expression::InfixExpression(infix_expression) => {
                self.eval_infix_expression(infix_expression)
            }
            Expression::IfExpression(if_expression) => self.eval_if_expression(if_expression),
            Expression::FunctionExpression(function_expression) => {
                self.eval_function_expression(function_expression)
            }
            Expression::FunctionCallExpression(function_call_expression) => {
                self.eval_function_call_expression(function_call_expression)
            }
        }
    }

    fn eval_identifier(&mut self, identifier: &Identifier) -> Option<Object> {
        match self.env.borrow().get(&identifier.value) {
            Some(val) => Some(val),
            None => None,
        }
    }

    fn eval_literal(&mut self, literal: &Literal) -> Option<Object> {
        match literal {
            Literal::Integer(i) => Some(Object::Literal(Integer(*i))),
            Literal::Boolean(b) => Some(Object::Literal(Boolean(*b))),
            Literal::String(s) => Some(Object::Literal(String(s.clone()))),
        }
    }

    fn eval_prefix_expression(&mut self, prefix_expression: &PrefixExpression) -> Option<Object> {
        let right = self.eval_expression(&prefix_expression.right);
        match right {
            Some(r) => self.eval_prefix_expression_operator(&prefix_expression.operator, &r),
            None => None,
        }
    }

    fn eval_prefix_expression_operator(
        &mut self,
        operator: &str,
        right: &Object,
    ) -> Option<Object> {
        match operator {
            "!" => match right {
                Object::Literal(Boolean(value)) => Some(Object::Literal(Boolean(!value))),
                Object::Null => Some(Object::Literal(Boolean(true))),
                _ => Some(Object::Literal(Boolean(false))),
            },
            "-" => match right {
                Object::Literal(Integer(value)) => Some(Object::Literal(Integer(-value))),
                _ => None,
            },
            _ => None,
        }
    }

    fn eval_infix_expression(&mut self, infix_expression: &InfixExpression) -> Option<Object> {
        let left = self.eval_expression(&infix_expression.left);
        let right = self.eval_expression(&infix_expression.right);
        match (left, right) {
            (Some(l), Some(r)) => {
                self.eval_infix_expression_operator(&infix_expression.operator, &l, &r)
            }
            _ => None,
        }
    }

    fn eval_infix_expression_operator(
        &mut self,
        operator: &str,
        left: &Object,
        right: &Object,
    ) -> Option<Object> {
        match (left, right) {
            (Object::Literal(Integer(l)), Object::Literal(Integer(r))) => {
                self.eval_integer_infix_expression(operator, l, r)
            }
            (Object::Literal(Boolean(l)), Object::Literal(Boolean(r))) => {
                self.eval_boolean_infix_expression(operator, l, r)
            }
            (Object::Literal(String(l)), Object::Literal(String(r))) => {
                self.eval_string_infix_expression(operator, l, r)
            }
            _ => None,
        }
    }

    fn eval_integer_infix_expression(
        &mut self,
        operator: &str,
        left: &i64,
        right: &i64,
    ) -> Option<Object> {
        let result = match operator {
            "+" => Object::Literal(Integer(left + right)),
            "-" => Object::Literal(Integer(left - right)),
            "*" => Object::Literal(Integer(left * right)),
            "/" => Object::Literal(Integer(left / right)),
            "<" => Object::Literal(Boolean(left < right)),
            ">" => Object::Literal(Boolean(left > right)),
            "==" => Object::Literal(Boolean(left == right)),
            "!=" => Object::Literal(Boolean(left != right)),
            ">=" => Object::Literal(Boolean(left >= right)),
            "<=" => Object::Literal(Boolean(left <= right)),
            _ => {
                return None;
            }
        };
        Some(result)
    }

    fn eval_boolean_infix_expression(
        &mut self,
        operator: &str,
        left: &bool,
        right: &bool,
    ) -> Option<Object> {
        match operator {
            "==" => Some(Object::Literal(Boolean(left == right))),
            "!=" => Some(Object::Literal(Boolean(left != right))),
            _ => None,
        }
    }

    fn eval_string_infix_expression(
        &mut self,
        operator: &str,
        left: &str,
        right: &str,
    ) -> Option<Object> {
        match operator {
            "+" => Some(Object::Literal(String(format!("{}{}", left, right)))),
            _ => None,
        }
    }

    fn eval_if_expression(&mut self, if_expression: &IfExpression) -> Option<Object> {
        let condition = self.eval_expression(&if_expression.condition);
        match condition {
            Some(Object::Literal(Boolean(b))) => {
                if b {
                    self.eval_statement(&if_expression.consequence)
                } else {
                    match &if_expression.alternative {
                        Some(alt) => self.eval_statement(alt),
                        None => Some(Object::Null),
                    }
                }
            }
            _ => None,
        }
    }

    fn eval_function_expression(
        &mut self,
        function_expression: &FunctionExpression,
    ) -> Option<Object> {
        let params = function_expression
            .parameters
            .clone()
            .iter()
            .map(|p| p.value.clone())
            .collect();
        let body = *function_expression.body.clone();
        Some(Object::Function(FunctionObject {
            parameters: params,
            body,
            env: Rc::clone(&self.env),
        }))
    }

    fn eval_function_call_expression(
        &mut self,
        function_call_expression: &FunctionCallExpression,
    ) -> Option<Object> {
        let function = self.eval_expression(&function_call_expression.function)?;
        let args = self.eval_expressions(&function_call_expression.arguments)?;
        self.apply_function(&function, &args)
    }

    fn apply_function(&mut self, function: &Object, args: &Vec<Object>) -> Option<Object> {
        match function {
            Object::Function(function) => {
                let old_env = Rc::clone(&self.env);
                let mut extended_env =
                    Environment::new_enclosed_environment(Rc::clone(&function.env));
                for (i, param) in function.parameters.iter().enumerate() {
                    extended_env.set(param.clone(), &args[i]);
                }
                self.env = Rc::new(RefCell::new(extended_env));
                let evaluated = self.eval_statement(&function.body);
                self.env = old_env;
                match evaluated {
                    Some(Object::Return(v)) => Some(*v),
                    _ => None,
                }
            }
            _ => {
                println!("Function call error");
                None
            }
        }
    }

    fn eval_expressions(&mut self, expressions: &Vec<Expression>) -> Option<Vec<Object>> {
        let mut result = Vec::new();
        for e in expressions {
            let evaluated = self.eval_expression(e);
            match evaluated {
                Some(v) => result.push(v),
                None => return None,
            }
        }
        Some(result)
    }
}
