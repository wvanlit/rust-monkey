use std::rc::Rc;
use core::cell::RefCell;
use std::collections::HashMap;
use crate::parser::ast;
use crate::evaluate::builtin::{BuiltinFunction, get_builtin_functions};



#[derive(Debug, Clone)]
pub enum Object {
	Integer(i32),
	Boolean(bool),
	String(String),
	Return(Box<Object>),
	Function{parameters: Vec<ast::Identifier>, body: ast::Statement, env: Rc<RefCell<Environment>>},
	Builtin(String, usize, BuiltinFunction),
	Null,
	Error(String)
}

impl Object {
	pub fn inspect(&self) -> String {
		match self {
			Object::Integer(i) => format!("{}", i),
			Object::Boolean(b) => format!("{}", b),
			Object::String(s) => s.clone(),
			Object::Return(r) => format!("return {}", r.inspect()),
			Object::Function{parameters, body, env: _} => {
				let mut output = String::new(); 
				for (index, ident) in parameters.iter().enumerate(){
					output += &ident;
					if parameters.len() - index != 1{
						output += ", ";
					}
				}
				format!("fn ({}) {{\n{}\n}}", output, ast::statement_to_string(body))
			},
			Object::Builtin(name, _, _) => format!("{}()", name),
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
			Object::String(_) => "STRING",
			Object::Return(_) => "RETURN",
			Object::Function{parameters: _, body: _, env: _} => "FUNCTION",
			Object::Builtin(_,_,_) => "BUILTIN",
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
	fn fill_builtins(&mut self){
		for (key, func) in get_builtin_functions().iter(){
			self.set(key.to_string(), func.clone());
		}
	}

	pub fn new() -> Self{
		let mut e = Environment{
			store: HashMap::new(),
			outer: None,
		};
		e.fill_builtins();
		e
	}

	pub fn new_outer(outer: Rc<RefCell<Self>>) -> Self{
		let mut e = Environment{
			store: HashMap::new(),
			outer: Some(outer),
		};
		e.fill_builtins();
		e
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
