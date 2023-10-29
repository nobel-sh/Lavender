use crate::ast;
use crate::ast::Expression;
use crate::environment::Environment;
use crate::object::{FunctionObject, Object};

pub fn eval(node: &ast::Node, env: &mut Environment) -> Option<Object> {
    match node {
        ast::Node::Program(p) => eval_program(p, env),
        ast::Node::Statement(s) => eval_statement(s, env),
        ast::Node::Expression(e) => eval_expression(e, env),
    }
}

pub fn eval_program(program: &ast::Program, env: &mut Environment) -> Option<Object> {
    let mut result = None;
    for s in &program.statements {
        result = eval_statement(s, env);
        match result {
            Some(Object::Return(val)) => return Some(Object::Return(val)),
            Some(_) => {}
            None => {}
        }
    }
    result
}

fn eval_statement(statement: &ast::Statement, env: &mut Environment) -> Option<Object> {
    match statement {
        ast::Statement::LetStatement(l) => {
            let val = eval_expression(&l.value, env);
            match val {
                Some(v) => env.set(l.name.value.clone(), &v),
                None => None,
            }
        }
        ast::Statement::ReturnStatement(r) => {
            let val = eval_expression(&r.return_value, env);
            match val {
                Some(v) => Some(Object::Return(Box::new(v))),
                None => None,
            }
        }
        ast::Statement::ExpressionStatement(e) => eval_expression(&e.expression, env),
        ast::Statement::BlockStatement(b) => eval_block_statement(b, env),
    }
}

fn eval_block_statement(block: &ast::BlockStatement, env: &mut Environment) -> Option<Object> {
    let mut result = None;
    for s in &block.statements {
        result = eval_statement(s, env);
        match result {
            Some(Object::Return(_)) => return result,
            Some(_) => {}
            None => {}
        }
    }
    result
}

fn eval_expression(expression: &ast::Expression, env: &mut Environment) -> Option<Object> {
    match expression {
        ast::Expression::Identifier(i) => eval_identifier(i, env),
        ast::Expression::Literal(l) => eval_literal(l),
        ast::Expression::PrefixExpression(p) => {
            let right = eval_expression(&p.right, env);
            match right {
                Some(r) => eval_prefix_expression(&p.operator, &r),
                None => None,
            }
        }
        ast::Expression::InfixExpression(i) => {
            let left = eval_expression(&i.left, env);
            let right = eval_expression(&i.right, env);
            match (left, right) {
                (Some(l), Some(r)) => eval_infix_expression(&i.operator, &l, &r),
                _ => None,
            }
        }
        ast::Expression::IfExpression(i) => eval_if_expression(i, env),
        ast::Expression::FunctionExpression(f) => eval_function_expression(f, env),
        ast::Expression::FunctionCallExpression(f) => eval_function_call_expression(f, env),
    }
}

fn eval_identifier(identifier: &ast::Identifier, env: &mut Environment) -> Option<Object> {
    match env.get(&identifier.value) {
        Some(val) => Some(val),
        None => None,
    }
}

fn eval_literal(literal: &ast::Literal) -> Option<Object> {
    match literal {
        ast::Literal::Integer(i) => Some(Object::Integer(*i)),
        ast::Literal::Boolean(b) => Some(Object::Boolean(*b)),
        ast::Literal::String(s) => Some(Object::String(s.clone())),
    }
}

fn eval_prefix_expression(operator: &str, right: &Object) -> Option<Object> {
    match operator {
        "!" => eval_bang_operator_expression(right),
        "-" => eval_minus_prefix_operator_expression(right),
        _ => None,
    }
}

fn eval_bang_operator_expression(right: &Object) -> Option<Object> {
    match right {
        Object::Boolean(b) => Some(Object::Boolean(!b)),
        Object::Null => Some(Object::Boolean(true)),
        _ => Some(Object::Boolean(false)),
    }
}

fn eval_minus_prefix_operator_expression(right: &Object) -> Option<Object> {
    match right {
        Object::Integer(i) => Some(Object::Integer(-i)),
        _ => None,
    }
}

fn eval_infix_expression(operator: &str, left: &Object, right: &Object) -> Option<Object> {
    match (left, right) {
        (Object::Integer(l), Object::Integer(r)) => eval_integer_infix_expression(operator, l, r),
        (Object::Boolean(l), Object::Boolean(r)) => eval_boolean_infix_expression(operator, l, r),
        (Object::String(l), Object::String(r)) => eval_string_infix_expression(operator, l, r),
        _ => None,
    }
}

fn eval_integer_infix_expression(operator: &str, left: &i64, right: &i64) -> Option<Object> {
    match operator {
        "+" => Some(Object::Integer(left + right)),
        "-" => Some(Object::Integer(left - right)),
        "*" => Some(Object::Integer(left * right)),
        "/" => Some(Object::Integer(left / right)),
        "<" => Some(Object::Boolean(left < right)),
        ">" => Some(Object::Boolean(left > right)),
        "==" => Some(Object::Boolean(left == right)),
        "!=" => Some(Object::Boolean(left != right)),
        _ => None,
    }
}

fn eval_boolean_infix_expression(operator: &str, left: &bool, right: &bool) -> Option<Object> {
    match operator {
        "==" => Some(Object::Boolean(left == right)),
        "!=" => Some(Object::Boolean(left != right)),
        _ => None,
    }
}

fn eval_string_infix_expression(operator: &str, left: &str, right: &str) -> Option<Object> {
    match operator {
        "+" => Some(Object::String(format!("{}{}", left, right))),
        _ => None,
    }
}

fn eval_if_expression(if_expression: &ast::IfExpression, env: &mut Environment) -> Option<Object> {
    let condition = eval_expression(&if_expression.condition, env);
    match condition {
        Some(Object::Boolean(b)) => {
            if b {
                eval_statement(&if_expression.consequence, env)
            } else {
                match &if_expression.alternative {
                    Some(alt) => eval_statement(alt, env),
                    None => Some(Object::Null),
                }
            }
        }
        _ => None,
    }
}

fn eval_function_expression(
    function_expression: &ast::FunctionExpression,
    env: &mut Environment,
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
        env: env.clone(),
    }))
}

fn eval_function_call_expression(
    function_call_expression: &ast::FunctionCallExpression,
    env: &mut Environment,
) -> Option<Object> {
    let function = eval_expression(&function_call_expression.function, env);
    let args = eval_expressions(&function_call_expression.arguments, env);
    match (function, args) {
        (Some(Object::Function(f)), Some(a)) => {
            let mut extended_env = Environment::new_enclosed_environment(f.env);
            let evaluated = eval_statement(&f.body, &mut extended_env);
            match evaluated {
                Some(Object::Return(v)) => Some(*v),
                _ => None,
            }
        }
        _ => None,
    }
}

fn eval_expressions(expressions: &Vec<Expression>, env: &mut Environment) -> Option<Vec<Object>> {
    let mut result = Vec::new();
    for e in expressions {
        let evaluated = eval_expression(e, env);
        match evaluated {
            Some(v) => result.push(v),
            None => return None,
        }
    }
    Some(result)
}
