use crate::parser::ast::*;
use crate::evaluate::object::*;

pub type BuiltinFunction = fn(Vec<Object>) -> Result<Object, String>;

pub fn get_builtin_functions() -> Vec<(Identifier, Object)>{
	vec![
		create_builtin("len", 1, builtin_len),
	]
}

fn create_builtin(name: &str, param_count: usize, function: BuiltinFunction) ->(Identifier, Object){
	(
		name.to_string(),
		Object::Builtin(name.to_string(), param_count, function)
	)
}


fn builtin_len(input: Vec<Object>) -> Result<Object, String>{
	let input_object = &input[0];
	match input_object {
		Object::String(s) => Ok(Object::Integer(s.len() as i32)),
		_ => Err(format!("argument to 'len' not supported, got {}", input_object.type_string())),
	}
}