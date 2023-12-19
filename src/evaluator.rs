use crate::ast::Expression;
use crate::ast::*;
use crate::environment::Environment;
use crate::object::Literal::{Boolean, Float, Integer, String}; //Direct import of enum variants
use crate::object::{FunctionObject, Object};
use std::cell::RefCell;
use std::rc::Rc;

pub struct Evaluator {
    env: Rc<RefCell<Environment>>,
}

type RustString = std::string::String;

pub type EvaluatorResult<T> = Result<T, EvaluatorError>;

#[derive(Debug, Clone)]
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

    pub fn eval(&mut self, program: &Program) -> EvaluatorResult<Object> {
        let mut result = Ok(Object::Null);
        for statement in &program.statements {
            result = self.eval_statement(statement);
            match result {
                Ok(Object::Return(v)) => return Ok(*v),
                Ok(_) => {}
                Err(e) => return Err(e),
            }
        }
        result
    }

    fn eval_statement(&mut self, statement: &Statement) -> EvaluatorResult<Object> {
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

    fn eval_let_statement(&mut self, let_statement: &LetStatement) -> EvaluatorResult<Object> {
        let val = self.eval_expression(&let_statement.value)?;
        let ident = Object::Identifier(let_statement.name.value.clone());
        self.env.borrow_mut().set(ident, &val);
        Ok(Object::Null)
    }

    fn eval_return_statement(
        &mut self,
        return_statement: &ReturnStatement,
    ) -> EvaluatorResult<Object> {
        let val = self.eval_expression(&return_statement.return_value);
        match val {
            Ok(v) => Ok(Object::Return(Box::new(v))),
            Err(e) => Err(e),
        }
    }

    fn eval_expression_statement(
        &mut self,
        expression_statement: &ExpressionStatement,
    ) -> EvaluatorResult<Object> {
        self.eval_expression(&expression_statement.expression)
    }

    fn eval_block_statement(
        &mut self,
        block_statement: &BlockStatement,
    ) -> EvaluatorResult<Object> {
        let mut result = Ok(Object::Null);
        for statement in &block_statement.statements {
            result = self.eval_statement(statement);
            match result {
                Ok(Object::Return(_)) | Err(_) => return result,
                Ok(_) => {}
            }
        }
        result
    }

    fn eval_expression(&mut self, expression: &Expression) -> EvaluatorResult<Object> {
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
            Expression::WhileExpression(while_expression) => {
                self.eval_while_expression(while_expression)
            }
            Expression::FunctionExpression(function_expression) => {
                self.eval_function_expression(function_expression)
            }
            Expression::FunctionCallExpression(function_call_expression) => {
                self.eval_function_call_expression(function_call_expression)
            }
        }
    }

    fn eval_identifier(&mut self, identifier: &Identifier) -> EvaluatorResult<Object> {
        match self.env.borrow().get(&identifier.value) {
            Ok(v) => Ok(v),
            Err(e) => Err(e),
        }
    }

    fn eval_literal(&mut self, literal: &Literal) -> EvaluatorResult<Object> {
        match literal {
            Literal::Integer(i) => Ok(Object::Literal(Integer(*i))),
            Literal::Float(f) => Ok(Object::Literal(Float(*f))),
            Literal::Boolean(b) => Ok(Object::Literal(Boolean(*b))),
            Literal::String(s) => Ok(Object::Literal(String(s.clone()))),
        }
    }

    fn eval_prefix_expression(
        &mut self,
        prefix_expression: &PrefixExpression,
    ) -> EvaluatorResult<Object> {
        let right = self.eval_expression(&prefix_expression.right);
        match right {
            Ok(r) => self.eval_prefix_expression_operator(&prefix_expression.operator, &r),
            Err(e) => Err(e),
        }
    }

    fn eval_prefix_expression_operator(
        &mut self,
        operator: &str,
        right: &Object,
    ) -> EvaluatorResult<Object> {
        match operator {
            "!" => match right {
                Object::Literal(Boolean(value)) => Ok(Object::Literal(Boolean(!value))),
                Object::Null => Ok(Object::Literal(Boolean(true))),
                _ => Ok(Object::Literal(Boolean(false))),
            },
            "-" => match right {
                Object::Literal(Integer(value)) => Ok(Object::Literal(Integer(-value))),
                Object::Literal(Float(value)) => Ok(Object::Literal(Float(-value))),
                _ => Err(EvaluatorError::new(format!(
                    "Cannot negate token : {}",
                    right
                ))),
            },
            _ => Err(EvaluatorError::new(format!(
                "Unknown operator: {}{}",
                operator, right
            ))),
        }
    }

    fn eval_infix_expression(
        &mut self,
        infix_expression: &InfixExpression,
    ) -> EvaluatorResult<Object> {
        let left = self.eval_expression(&infix_expression.left)?;
        let right = self.eval_expression(&infix_expression.right)?;
        let operator = &infix_expression.operator;
        if operator == "=" {
            return self
                .eval_assignment_expression(&infix_expression.left, &infix_expression.right);
        }
        self.eval_infix_expression_operator(&operator, &left, &right)
    }

    fn eval_infix_expression_operator(
        &mut self,
        operator: &str,
        left: &Object,
        right: &Object,
    ) -> EvaluatorResult<Object> {
        match (left, right) {
            (Object::Literal(Integer(l)), Object::Literal(Integer(r))) => {
                self.eval_integer_infix_expression(operator, l, r)
            }
            (Object::Literal(Float(l)), Object::Literal(Float(r))) => {
                self.eval_float_infix_expression(operator, l, r)
            }
            (Object::Literal(Boolean(l)), Object::Literal(Boolean(r))) => {
                self.eval_boolean_infix_expression(operator, l, r)
            }
            (Object::Literal(String(l)), Object::Literal(String(r))) => {
                self.eval_string_infix_expression(operator, l, r)
            }
            _ => Err(EvaluatorError::new(format!(
                "Illegal operation : {} {} {}",
                left, operator, right
            ))),
        }
    }

    fn eval_assignment_expression(
        &mut self,
        left: &Box<Expression>,
        right: &Box<Expression>,
    ) -> EvaluatorResult<Object> {
        let right = self.eval_expression(&right)?;
        match *left.clone() {
            Expression::Identifier(identifier) => {
                let ident = Object::Identifier(identifier.value.clone());
                self.env.borrow_mut().set(ident, &right);
                Ok(right)
            }
            _ => Err(EvaluatorError::new(format!(
                "Illegal assignment target: {}",
                left
            ))),
        }
    }

    fn eval_integer_infix_expression(
        &mut self,
        operator: &str,
        left: &i64,
        right: &i64,
    ) -> EvaluatorResult<Object> {
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
                return Err(EvaluatorError::new(format!(
                    "Illegal Operation: {} {} {}",
                    left, operator, right
                )));
            }
        };
        Ok(result)
    }

    //TODO: remove code duplication
    fn eval_float_infix_expression(
        &mut self,
        operator: &str,
        left: &f64,
        right: &f64,
    ) -> EvaluatorResult<Object> {
        let result = match operator {
            "+" => Object::Literal(Float(left + right)),
            "-" => Object::Literal(Float(left - right)),
            "*" => Object::Literal(Float(left * right)),
            "/" => Object::Literal(Float(left / right)),
            "<" => Object::Literal(Boolean(left < right)),
            ">" => Object::Literal(Boolean(left > right)),
            "==" => Object::Literal(Boolean(left == right)),
            "!=" => Object::Literal(Boolean(left != right)),
            ">=" => Object::Literal(Boolean(left >= right)),
            "<=" => Object::Literal(Boolean(left <= right)),
            _ => {
                return Err(EvaluatorError::new(format!(
                    "Unknown operator: {} {} {}",
                    left, operator, right
                )));
            }
        };
        Ok(result)
    }

    fn eval_boolean_infix_expression(
        &mut self,
        operator: &str,
        left: &bool,
        right: &bool,
    ) -> EvaluatorResult<Object> {
        match operator {
            "==" => Ok(Object::Literal(Boolean(left == right))),
            "!=" => Ok(Object::Literal(Boolean(left != right))),
            _ => Err(EvaluatorError::new(format!(
                "Unknown operator: {} {} {}",
                left, operator, right
            ))),
        }
    }

    fn eval_string_infix_expression(
        &mut self,
        operator: &str,
        left: &str,
        right: &str,
    ) -> EvaluatorResult<Object> {
        match operator {
            "+" => Ok(Object::Literal(String(format!("{}{}", left, right)))),
            _ => Err(EvaluatorError::new(format!(
                "Unknown operator: {} {} {}",
                left, operator, right
            ))),
        }
    }

    fn eval_if_expression(&mut self, if_expression: &IfExpression) -> EvaluatorResult<Object> {
        let condition = self.eval_expression(&if_expression.condition);
        match condition {
            Ok(Object::Literal(Boolean(b))) => {
                if b {
                    self.eval_statement(&if_expression.consequence)
                } else {
                    match &if_expression.alternative {
                        Some(alt) => self.eval_statement(alt),
                        None => Ok(Object::Null),
                    }
                }
            }
            Ok(_) => Err(EvaluatorError::new(format!(
                "Condition not a boolean: {}",
                if_expression.condition
            ))),
            Err(e) => Err(e),
        }
    }

    fn eval_while_expression(
        &mut self,
        while_expression: &WhileExpression,
    ) -> EvaluatorResult<Object> {
        let mut result = Ok(Object::Null);
        while let Ok(Object::Literal(Boolean(b))) =
            self.eval_expression(&while_expression.condition)
        {
            if b {
                result = self.eval_statement(&while_expression.body);
            } else {
                break;
            }
        }
        result
    }

    fn eval_function_expression(
        &mut self,
        function_expression: &FunctionExpression,
    ) -> EvaluatorResult<Object> {
        let params = function_expression
            .parameters
            .clone()
            .iter()
            .map(|p| p.value.clone())
            .collect();
        let body = *function_expression.body.clone();
        Ok(Object::Function(FunctionObject {
            parameters: params,
            body,
            env: Rc::clone(&self.env),
        }))
    }

    fn eval_function_call_expression(
        &mut self,
        function_call_expression: &FunctionCallExpression,
    ) -> EvaluatorResult<Object> {
        let function = self.eval_expression(&function_call_expression.function)?;
        let args = self.eval_expressions(&function_call_expression.arguments)?;
        self.apply_function(&function, &args)
    }

    fn apply_function(&mut self, function: &Object, args: &Vec<Object>) -> EvaluatorResult<Object> {
        match function {
            Object::Function(function) => {
                let old_env = Rc::clone(&self.env);
                let mut extended_env =
                    Environment::new_enclosed_environment(Rc::clone(&function.env));
                for (i, param) in function.parameters.iter().enumerate() {
                    let ident = Object::Identifier(param.clone());
                    extended_env.set(ident, &args[i]);
                }
                self.env = Rc::new(RefCell::new(extended_env));
                let evaluated = self.eval_statement(&function.body);
                self.env = old_env;
                match evaluated {
                    Ok(Object::Return(v)) => Ok(*v),
                    Ok(_) => Ok(Object::Null),
                    Err(e) => Err(e),
                }
            }
            _ => Err(EvaluatorError::new(format!("Not a function: {}", function))),
        }
    }

    fn eval_expressions(&mut self, expressions: &Vec<Expression>) -> EvaluatorResult<Vec<Object>> {
        let mut result = Vec::new();
        for e in expressions {
            let evaluated = self.eval_expression(e);
            match evaluated {
                Ok(v) => result.push(v),
                Err(e) => return Err(e),
            }
        }
        Ok(result)
    }
}
