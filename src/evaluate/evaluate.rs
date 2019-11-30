use std::rc::Rc;
use std::cell::RefCell;
use crate::parser::ast;
use crate::evaluate::object::{Object, Environment};
use crate::tokens::tokens::{TokenType};

#[derive(Debug)]
pub struct Evaluator {
	environment: Rc<RefCell<Environment>>,
}

impl Evaluator {
	pub fn new() -> Self{
		Evaluator{
			environment: Rc::new(RefCell::new(Environment::new())),
		}
	}

	pub fn new_with_env(env: Environment) -> Self{
		Evaluator{
			environment: Rc::new(RefCell::new(env)),
		}
	}
}

impl Evaluator {
	// add code here
	pub fn eval_program(&mut self, program: &mut ast::Program) -> Object{
		match program.statements.len() {
			0 => Object::Null,
			1 => {
				let obj = self.eval_statement(&mut program.statements.remove(0));
				match obj{
					Object::Return(object) => return *object,
					_ => obj,
				}
			},
			_ => self.eval_statements(&mut program.statements)
		}
	}

	fn eval_statement(&mut self, statement: &mut ast::Statement) -> Object{
		match statement {
			ast::Statement::Expr(expr) => self.eval_expression(expr),
			ast::Statement::BlockStatement(vec) => self.eval_statements(vec),
			ast::Statement::Return(expr) => self.eval_return_statement(expr),
			ast::Statement::Let(ident, expr) => {
				let obj = self.eval_expression(expr);
				self.eval_let_statement(ident.clone(), obj)
			},
			_ => Object::Null,
		}
	}

	fn eval_statements(&mut self, statements: &mut Vec<ast::Statement>) -> Object{
		let mut result = Object::Null;

		for statement in statements.iter_mut() {
			result = self.eval_statement(statement);

			match result {
				Object::Return(obj) => return *obj,
				Object::Error(_) => return result,
				_ => (),
			};
		}

		result
	}

	fn eval_return_statement(&mut self, expr: &mut ast::Expr) -> Object{
		let val = self.eval_expression(expr);
		if val.is_error(){
			val
		}else{
			val.return_object()
		}
	}

	fn eval_let_statement(&mut self, identifier: String, object: Object) -> Object{
		if object.is_error(){
			return object;
		}

		let mut env = self.environment.borrow_mut();
		env.set(identifier, object.clone());
		Object::Null
	}

	fn eval_expression(&mut self, expression: &mut ast::Expr) -> Object{
		match expression{
			ast::Expr::IntegerLiteral(i) => Object::Integer(*i),
			ast::Expr::Bool(b) => Object::Boolean(*b),
			ast::Expr::String(s) => Object::String(s.clone()),
			ast::Expr::Identifier(ident) => self.eval_identifier(ident.clone()),
			ast::Expr::Prefix(op, right) => {
				let right_obj = self.eval_expression(right);
				self.eval_prefix_expression(op, right_obj)
			},
			ast::Expr::Infix(op, left, right) => {
				let left_obj = self.eval_expression(left);
				let right_obj = self.eval_expression(right);
				self.eval_infix_expression(op, left_obj, right_obj)
			},
			ast::Expr::If(condition, then, else_option) => self.eval_if_expression(condition, then, else_option),
			ast::Expr::FunctionLiteral(parameters, body) => Object::Function{parameters: parameters.to_vec(), body: *body.clone(), env: self.environment.clone()},
			ast::Expr::CallExpression{function, arguments} => self.eval_call_expression(function, arguments),
			_ => Object::Null,
		}
	}

	fn eval_expressions(&mut self, expressions: &mut Vec<ast::Expr>) -> Vec<Object>{
		let mut result = vec![];
		for expression in expressions {
			let obj = self.eval_expression(expression);
			if obj.is_error(){
				return vec![obj];
			}
			result.push(obj);
		}
		result
	}

	fn eval_identifier(&mut self, identifier: String) -> Object{
		let env = self.environment.borrow();
		let optional = env.get(identifier.clone());
		match optional {
			Some(obj) => obj.clone(),
			None => Object::Error(format!("identifier not found: {}", identifier)),
		}
	}

	fn eval_prefix_expression(&self, operator: &String, right: Object) -> Object{
		if right.is_error(){
			return right;
		}

		match operator.as_str() {
			"!" => self.eval_bang_operator_expression(right),
			"-" => self.eval_minus_operator_expression(right),
			_ => Object::Error(format!("unknown operator: {}{}", operator, right.type_string())),
		}
	}

	fn eval_bang_operator_expression(&self, right: Object) -> Object{
		match right {
			Object::Boolean(val) => Object::Boolean(!val),
			Object::Integer(i) => Object::Boolean(i == 0),
			_ => Object::Error(format!("unknown operator: !{}", right.type_string())),
		}
	}

	fn eval_minus_operator_expression(&self, right: Object) -> Object{
		match right {
			Object::Integer(i) => Object::Integer(-i),
			_ => Object::Error(format!("unknown operator: -{}", right.type_string())),
		}
	}

	fn eval_infix_expression(&self, operator: &TokenType, left: Object, right: Object) -> Object{
		if left.is_error(){
			return left;
		}else if right.is_error(){
			return right;
		}

		let type_error = Object::Error(format!("type mismatch: {} {} {}", left.type_string(), operator, right.type_string()));

		match left {
			Object::Integer(i) => match right {
				Object::Integer(j) => self.eval_infix_integer_expression(operator, i, j),
				_ => type_error,
			},
			Object::Boolean(b1) => match right {
				Object::Boolean(b2) => self.eval_infix_bool_expression(operator, b1, b2),
				_ => type_error,
			}
			Object::String(s1) => match right {
				Object::String(s2) => self.eval_infix_string_expression(operator, s1, s2),
				_ => type_error,
			}

			_ => type_error,
		}
	}

	fn eval_infix_integer_expression(&self, operator: &TokenType, left_val: i32, right_val: i32) -> Object{
		match operator {
			// Mathematical
			TokenType::PLUS => Object::Integer(left_val + right_val),
			TokenType::MINUS => Object::Integer(left_val - right_val),
			TokenType::ASTERISK => Object::Integer(left_val * right_val),
			TokenType::SLASH => Object::Integer(left_val / right_val),
			// Equality
			TokenType::LT => Object::Boolean(left_val < right_val),
			TokenType::GT => Object::Boolean(left_val > right_val),
			TokenType::EQ => Object::Boolean(left_val == right_val),
			TokenType::NOT_EQ => Object::Boolean(left_val != right_val),
			_ => Object::Null,
		}
	}

	fn eval_infix_bool_expression(&self, operator: &TokenType, left_val: bool, right_val: bool) -> Object{
		match operator{
			TokenType::EQ => Object::Boolean(left_val == right_val),
			TokenType::NOT_EQ => Object::Boolean(left_val != right_val),
			_ => Object::Error(format!("unknown operator: BOOLEAN {} BOOLEAN", operator)),
		}
	}

	fn eval_infix_string_expression(&self, operator: &TokenType, left_val: String, right_val: String) -> Object{
		match operator{
			TokenType::PLUS => Object::String(left_val + &right_val),
			_ => Object::Error(format!("unknown operator: STRING {} STRING", operator)),
		}
	}

	fn eval_if_expression(&mut self, mut condition: &mut Box<ast::Expr>, mut then: &mut Box<ast::Statement>, else_option: &mut Option<Box<ast::Statement>>) -> Object{
		let condition_object = self.eval_expression(&mut condition);
		if condition_object.is_error(){
			return condition_object;
		}

		if self.is_truthy(condition_object){
			self.eval_statement(&mut then)
		}else{
			match else_option {
				Some(stmnt) => self.eval_statement(stmnt),
				None => Object::Null,
			}
		}
	}

	fn eval_call_expression(&mut self, function: &mut Box<ast::Expr>, arguments: &mut Vec<ast::Expr>) -> Object{
		let function_object = self.eval_expression(function);
		if function_object.is_error(){
			return function_object;
		}

		let args = self.eval_expressions(arguments);
		if args.len() == 1 && args[0].is_error() {
			return args[0].clone()
		}

		self.apply_function(function_object, args)
	}

	fn apply_function(&mut self, function_object: Object, args: Vec<Object>) -> Object{
		match function_object {
			Object::Function{parameters, mut body, env} => {
				if parameters.len() != args.len() {
					return Object::Error(format!("wrong number of arguments, got {} instead of {}", args.len(), parameters.len()))
				}
				let extended_env = self.extend_environment(env, parameters, args);
				let mut evaluator = Evaluator::new_with_env(extended_env);
				let eval = evaluator.eval_statement(&mut body);
				match eval {
					Object::Return(val) => *val,
					_ => eval,
				}
			},
			Object::Builtin(_, paramcount, func) => {
				if paramcount != args.len() {
					return Object::Error(format!("wrong number of arguments, got {} instead of {}", args.len(), paramcount))
				};
				match func(args){
					Ok(obj) => obj,
					Err(e) => Object::Error(e),
				}
			},
			_ => Object::Error(format!("not a function: {}", function_object.type_string())),
		}
	}

	fn extend_environment(&mut self, outer: Rc<RefCell<Environment>>, params: Vec<ast::Identifier>, args: Vec<Object>) -> Environment{
		let mut env = Environment::new_outer(outer);
		for (index, parameter) in params.iter().enumerate(){
			env.set(parameter.clone(), args[index].clone());
		}

		env
	}

	fn is_truthy(&self, object: Object) -> bool{
		match object {
			Object::Boolean(b) => b,
			Object::Null => false,
			_ => match self.object_to_bool_object(object) {
				Object::Boolean(b) => b,
				_ => false,
			},
		}
	}

	fn object_to_bool_object(&self, object: Object) -> Object {
		match object {
			Object::Integer(i) => Object::Boolean(i != 0),
			_ => Object::Null,
		}
}

}
#[cfg(test)]
mod tests {
	use super::*;
	use crate::lexer::lexer::{Lexer};
	use crate::parser::parser::{Parser};


	fn test_eval(input: String) -> Object{
		let l = Lexer::new(input);
		let mut p = Parser::new(l);
		let mut e = Evaluator::new();
		e.eval_program(&mut p.parse_program())
	}

	fn test_integer_object(object: Object, expected: i32){
		match object {
			Object::Integer(i) => assert_eq!(i,expected),
			_ => assert!(false, "{:?} is not an integer!", object),
		}
	}

	fn test_boolean_object(object: Object, expected: bool){
		match object {
			Object::Boolean(b) => assert_eq!(b,expected),
			_ => assert!(false, "{:?} is not a boolean!", object),
		}
	}

	fn test_object(object: Object, expected: &Object){
		match expected {
			Object::Integer(i) => test_integer_object(object, *i),
			Object::Boolean(b) => test_boolean_object(object, *b),
			Object::Null => match object {
				Object::Null => (),
				_ => assert!(false, "Expected Null, got {:?}", object),
			}
			_ => assert!(false, "{:?} not in test_object", expected),
		}
	}

	#[test]
	fn test_eval_integer_expression() {
		let input = vec![
		("5", 5),
		("10", 10),
		("-5", -5),
		("-10", -10),
		("5 + 5 - 2", 8),
		("2 * 2 * 2 * 2", 16),
		("-50 + 70", 20),
		("5 * 5 + 2", 27),
		("5 + 5 * 2", 15),
		("20 / 5 * 2", 8),
		("4 * (2 + 10)", 48),
		("(20 + 60) / (14 - 4)", 8),

		];

		for test in input.iter() {
			let evaluated = test_eval(test.0.to_string());
			test_integer_object(evaluated, test.1);
		}
	}

	#[test]
	fn test_eval_boolean_expression() {
		let input = vec![
		// Standard
		("true", true),
		("false", false),
		// Bang Prefix
		("!true", false),
		("!false", true),
		("!5", false),
		("!!true", true),
		("!!false", false),
		("!!5", true),
		// Equality
		("1 < 2", true),
		("1 > 2", false),
		("2 < 1", false),
		("2 > 1", true),
		("1 == 1", true),
		("1 == 2", false),
		("1 != 1", false),
		("1 != 2", true),
		("true == true", true),
		("true != true", false),
		("true == false", false),
		("true != false", true),
		("(2 == 2) != (true == false)", true),
		];

		for test in input.iter() {
			let evaluated = test_eval(test.0.to_string());
			test_boolean_object(evaluated, test.1);
		}
	}

	fn int(i: i32) -> Object {
		Object::Integer(i)
	}

	fn null() -> Object{
		Object::Null
	}

	#[test]
	fn test_eval_conditional_expression() {
		let input = vec![
		("if (true) { 10 }", int(10)),
		("if (false) { 10 }", null()),
		("if (1) { 10 }", int(10)),
		("if (1 < 2) { 10 }", int(10)),
		("if (1 > 2) { 10 }", null()),
		("if (1 > 2) { 10 } else { 20 }", int(20)),
		("if (1 < 2) { 10 } else { 20 }", int(10)),

		];

		for test in input.iter() {
			println!("{:?}", test);
			let evaluated = test_eval(test.0.to_string());
			test_object(evaluated, &test.1);
		}
	}

	#[test]
	fn test_return_expression() {
		let input = vec![
		("return 5;", 5),
		("2; return 15;", 15),
		("return 10; 7;", 10),
		("return -5;", -5),
		("return 2 * -5; 6;", -10),
		("if (10 > 1) {
			if (10 > 1) { 8; return 10; 6; }
			else{ return 5;}
		}else{ return 1; }", 10),
		];

		println!("Start Test!");
		for test in input.iter() {
			println!("{:?}", test);
			let evaluated = test_eval(test.0.to_string());
			test_integer_object(evaluated, test.1);
		}
	}

	#[test]
	fn test_error_handling() {
		let input = vec![
		("5 + true;", "ERROR: type mismatch: INTEGER + BOOLEAN"),
		("5 + true; 5;", "ERROR: type mismatch: INTEGER + BOOLEAN"),
		("5; 5 + true;", "ERROR: type mismatch: INTEGER + BOOLEAN"),
		("-true", "ERROR: unknown operator: -BOOLEAN"),
		("true + false;", "ERROR: unknown operator: BOOLEAN + BOOLEAN"),
		("5; false + true; 5", "ERROR: unknown operator: BOOLEAN + BOOLEAN"),
		("if (10 > 1) {
			if (10 > 1) { return true + false; }
		}else{ return 1; }", "ERROR: unknown operator: BOOLEAN + BOOLEAN"),
		("foo", "ERROR: identifier not found: foo"),
		(r#" "hello" + 1 "#, "ERROR: type mismatch: STRING + INTEGER"),
		(r#" "hello world" - "world" "#, "ERROR: unknown operator: STRING - STRING"),

		];

		println!("Start Test!");
		for test in input.iter() {
			let evaluated = test_eval(test.0.to_string());
			assert_eq!(evaluated.inspect(), test.1);
		}
	}

	#[test]
	fn test_let_statement() {
		let input = vec![
		("let a = 5; a;", 5),
		("let a = 5 * 5; a;", 25),
		("let a = 5; let b = a; b;", 5),
		("let a = 5; let b = a; let c = a + b + 10; c;", 20),
		];

		for test in input.iter() {
			let evaluated = test_eval(test.0.to_string());
			test_integer_object(evaluated, test.1);
		}
	}

	#[test]
	fn test_function_object() {
		let input = "fn(x,y){x+y;};";

		let evaluated = test_eval(input.to_string());
		
		match evaluated {
			Object::Function{mut parameters, body, env: _} => {
				assert_eq!(parameters.len(), 2);
				assert_eq!(parameters.pop().expect("expected value"), "y");
				assert_eq!(parameters.pop().expect("expected value"), "x");

				assert_eq!(ast::statement_to_string(&body), "(x + y)");
			},
			_ => assert!(false),
		}
	}

	#[test]
	fn test_function_call() {
		let input = vec![
		("let identity = fn(x){x;}; identity(5);", 5),
		("let identity = fn(x){return x;}; identity(5);", 5),
		("let double = fn(x){x*2;}; double(5);", 10),
		("let add = fn(x,y){x+y;}; add(5, 2);", 7),
		("let add = fn(x,y){x+y;}; add(3+4, add(1,2));", 10),
		("fn(x){x;}(5);", 5),
		("let add = fn (x) { fn(y){return x+y; }; }; let addFive = add(5); addFive(2)", 7),
		];

		for test in input.iter() {
			let evaluated = test_eval(test.0.to_string());
			test_integer_object(evaluated, test.1);
		}
	}

	#[test]
	fn test_string_literal(){
		let input = r#" "Hello World!" "#;

		let evaluated = test_eval(input.to_string());
		match evaluated {
			Object::String(s) => assert_eq!(s, "Hello World!".to_string()),
			_ => assert!(false, "Expected String Object, go {:?} instead!", evaluated),
		}
	}

	#[test]
	fn test_string_literal_concat(){
		let input = r#" "Hello" + " " + "World!" "#;

		let evaluated = test_eval(input.to_string());
		match evaluated {
			Object::String(s) => assert_eq!(s, "Hello World!".to_string()),
			_ => assert!(false, "Expected String Object, go {:?} instead!", evaluated),
		}
	}

	fn error(message: &str) -> Object{
		Object::Error(message.to_string())
	}

	#[test]
	fn test_builtin_functions() {
		let input = vec![
		(r#"len("")"#, int(0)),
		(r#"len("hello")"#, int(5)),
		(r#"len("hello world")"#, int(11)),
		(r#"len(1)"#, error("argument to 'len' not supported, got INTEGER")),
		(r#"len("hello", "world")"#, error("wrong number of arguments, got 2 instead of 1")),
		];

		for test in input.iter() {
			let evaluated = test_eval(test.0.to_string());
			match &test.1 {
				Object::Error(m) => match evaluated{
					Object::Error(m2) => assert_eq!(*m, m2),
					_ => assert!(false, "Expected [{}] but got [{}]!", test.1.inspect(), evaluated.inspect()),
				},
				Object::Integer(i) => test_integer_object(evaluated, *i),
				_ => assert!(false, "Type {} not supported in test!", test.1.type_string()),
			}
			
		}
	}
}