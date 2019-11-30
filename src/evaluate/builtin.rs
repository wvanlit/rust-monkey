use crate::evaluate::object::*;
use crate::parser::ast::*;

pub type BuiltinFunction = fn(Vec<Object>) -> Result<Object, String>;

pub fn get_builtin_functions() -> Vec<(Identifier, Object)> {
    vec![
        create_builtin("len", 1, builtin_len),
        create_builtin("first", 1, builtin_first),
        create_builtin("last", 1, builtin_last),
        create_builtin("rest", 1, builtin_rest),
        create_builtin("puts", -1, builtin_puts),
    ]
}

fn create_builtin(name: &str, param_count: i32, function: BuiltinFunction) -> (Identifier, Object) {
    (
        name.to_string(),
        Object::Builtin(name.to_string(), param_count, function),
    )
}

fn builtin_len(input: Vec<Object>) -> Result<Object, String> {
    let input_object = &input[0];
    match input_object {
        Object::Array(elem) => Ok(Object::Integer(elem.len() as i32)),
        Object::String(s) => Ok(Object::Integer(s.len() as i32)),
        _ => Err(format!(
            "argument to 'len' not supported, got {}",
            input_object.type_string()
        )),
    }
}

fn builtin_first(input: Vec<Object>) -> Result<Object, String> {
    let input_object = &input[0];
    match input_object {
        Object::Array(elem) => {
            if elem.len() > 0 {
                Ok(elem[0].clone())
            } else {
                Ok(Object::Null)
            }
        }
        _ => Err(format!(
            "argument to 'first' not supported, got {}",
            input_object.type_string()
        )),
    }
}

fn builtin_last(input: Vec<Object>) -> Result<Object, String> {
    let input_object = &input[0];
    match input_object {
        Object::Array(elem) => {
            if elem.len() > 0 {
                Ok(elem[elem.len() - 1].clone())
            } else {
                Ok(Object::Null)
            }
        }
        _ => Err(format!(
            "argument to 'last' not supported, got {}",
            input_object.type_string()
        )),
    }
}

fn builtin_rest(input: Vec<Object>) -> Result<Object, String> {
    let input_object = &input[0];
    match input_object {
        Object::Array(elem) => {
            if elem.len() > 0 {
                let mut new_elem = elem.clone();
                new_elem.remove(0);
                Ok(Object::Array(new_elem))
            } else {
                Ok(Object::Null)
            }
        }
        _ => Err(format!(
            "argument to 'rest' not supported, got {}",
            input_object.type_string()
        )),
    }
}

fn builtin_puts(input: Vec<Object>) -> Result<Object, String> {
    for object in input.iter() {
        println!("{}", object.inspect());
    }

    Ok(Object::Null)
}
