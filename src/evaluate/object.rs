use std::rc::Rc;
use core::cell::RefCell;
use std::collections::HashMap;
use crate::parser::ast;

#[derive(Debug, Clone)]
pub enum Object {
	Integer(i32),
	Boolean(bool),
	Return(Box<Object>),
	Function{parameters: Vec<ast::Identifier>, body: ast::Statement, env: Rc<RefCell<Environment>>},
	Null,
	Error(String)
}

impl Object {
	pub fn inspect(&self) -> String {
		match self {
			Object::Integer(i) => format!("{}", i),
			Object::Boolean(b) => format!("{}", b),
			Object::Return(r) => format!("return {}", r.inspect()),
			Object::Function{parameters, body, env: _} => {
				let mut output = String::new(); 
				for (index, ident) in parameters.iter().enumerate(){
					output += &ident.value;
					if parameters.len() - index != 1{
						output += ", ";
					}
				}
				format!("fn ({}) {{\n{}\n}}", output, ast::statement_to_string(body))
			},
			Object::Null => "NULL".to_string(),
			Object::Error(message) => format!("ERROR: {}", message),
		}
	}
	
	pub fn return_object(self) -> Object{
		Object::Return(Box::new(self))
	}

	pub fn is_error(&self) -> bool{
		match self {
			Object::Error(_) => true,
			_ => false,
		}
	}

	pub fn type_string(&self) -> &str{
		match self {
			Object::Integer(_) => "INTEGER",
			Object::Boolean(_) => "BOOLEAN",
			Object::Return(_) => "RETURN",
			Object::Function{parameters: _, body: _, env: _} => "FUNCTION",
			Object::Null => "NULL",
			Object::Error(_) => "ERROR",
		}
	}
}

#[derive(Debug, Clone)]
pub struct Environment {
	store: HashMap<String, Object>,
	outer: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
	pub fn new() -> Self{
		Environment{
			store: HashMap::new(),
			outer: None,
		}
	}

	pub fn new_outer(outer: Rc<RefCell<Self>>) -> Self{
		Environment{
			store: HashMap::new(),
			outer: Some(outer),
		}
	}

	pub fn get(&self, key: String) -> Option<Object>{
		match self.store.get(&key){
			Some(obj) => Some(obj.clone()),
			None => {
				match &self.outer{
					Some(env) => env.borrow_mut().get(key).clone(),
					None => return None,
				}
			}
		}
	}

	pub fn set(&mut self, key: String, value: Object){
		self.store.insert(key, value);
	}
}