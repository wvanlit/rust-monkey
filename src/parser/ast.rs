use crate::tokens::tokens::{Token, TokenType};

pub struct Program {
    pub statements: Vec<Statement>,
}

pub fn program_to_string(program: &Program) -> String{
	let mut output = String::new();
	for statement in program.statements.iter(){
		output += &statement_to_string(&statement);
	}

	output
}

pub enum Statement {
    Let(Identifier, Expr),
    Return(Expr),
    Expr(Expr),
    None,
}

pub fn statement_to_string(statement: &Statement) -> String{
	match &*statement {
		Statement::Let(ident, expr) => format!("let {} = {}", ident.value, expression_to_string(expr) ),
		Statement::Return(expr) => format!("(return {})", expression_to_string(expr)),
		Statement::Expr(expr) => format!("{}", expression_to_string(expr)),
		Statement::None => "None".to_string(),
	}
}

pub enum Expr{
    Identifier(Identifier),
    IntegerLiteral(i32),
    Prefix(String, Box<Expr>),
    Infix(TokenType, Box<Expr>, Box<Expr>),
    None,
}

pub fn expression_to_string(expression: &Expr) -> String{
	match expression {
		Expr::Identifier(id) => format!("{}", id.value),
		Expr::IntegerLiteral(i) => format!("{}", i),
		Expr::Prefix(op, boxed_expr) => format!("({}{})",op, expression_to_string(boxed_expr)),
		Expr::Infix(op, boxed_left, boxed_right) => format!("({} {} {})", expression_to_string(boxed_left), op, expression_to_string(boxed_right)),
		Expr::None => "none".to_string(),
	}
}

pub struct Identifier {
    pub value: String,
    pub token: Token,
}
