use crate::tokens::tokens::TokenType;

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub statements: Vec<Statement>,
}

pub fn program_to_string(program: &Program) -> String {
    let mut output = String::new();
    for (index, statement) in program.statements.iter().enumerate() {
        output += &statement_to_string(&statement);
        if program.statements.len() - index != 1 {
            output += "\n";
        }
    }

    output
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Statement {
    Let(Identifier, Expr),
    Return(Expr),
    BlockStatement(Vec<Statement>),
    Expr(Expr),
    None,
}

pub fn statement_to_string(statement: &Statement) -> String {
    match &*statement {
        Statement::Let(ident, expr) => format!("let {} = {}", ident, expression_to_string(expr)),
        Statement::Return(expr) => format!("(return {})", expression_to_string(expr)),
        Statement::Expr(expr) => format!("{}", expression_to_string(expr)),
        Statement::BlockStatement(vec) => {
            let mut output = String::new();
            for (index, stmnt) in vec.iter().enumerate() {
                output += &statement_to_string(stmnt);
                if vec.len() - index != 1 {
                    output += "\n";
                }
            }
            output
        }
        Statement::None => "None".to_string(),
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Bool(bool),
    Identifier(Identifier),
    IntegerLiteral(i32),
    String(String),
    ArrayLiteral(Vec<Expr>),
    Index(Box<Expr>, Box<Expr>), // Left, Index
    HashLiteral(Vec<(Expr, Expr)>),
    Prefix(String, Box<Expr>),
    Infix(TokenType, Box<Expr>, Box<Expr>),
    If(Box<Expr>, Box<Statement>, Option<Box<Statement>>),
    FunctionLiteral(Vec<Identifier>, Box<Statement>),
    CallExpression {
        function: Box<Expr>,
        arguments: Vec<Expr>,
    }, // Function can be either func_literal or identifier
    None,
}

pub fn expression_to_string(expression: &Expr) -> String {
    match expression {
        Expr::Bool(val) => format!("{}", val),
        Expr::Identifier(id) => format!("{}", id),
        Expr::IntegerLiteral(i) => format!("{}", i),
        Expr::String(s) => s.clone(),
        Expr::Index(l, i) => format!("({}[{}])", expression_to_string(l), expression_to_string(i)),
        Expr::ArrayLiteral(vec) => {
            let mut output = String::new();
            for (index, expr) in vec.iter().enumerate() {
                output += &expression_to_string(expr);
                if vec.len() - index != 1 {
                    output += ", ";
                }
            }
            format!("[{}]", output)
        }
        Expr::HashLiteral(vec) => {
            let mut output = String::new();
            for (index, (key, expr)) in vec.iter().enumerate() {
                output += format!(
                    "{}:{}",
                    expression_to_string(key).as_str(),
                    expression_to_string(expr).as_str()
                )
                .as_str();
                if vec.len() - index != 1 {
                    output += ", ";
                }
            }
            format!("{{{}}}", output)
        }
        Expr::Prefix(op, boxed_expr) => format!("({}{})", op, expression_to_string(boxed_expr)),
        Expr::Infix(op, boxed_left, boxed_right) => format!(
            "({} {} {})",
            expression_to_string(boxed_left),
            op,
            expression_to_string(boxed_right)
        ),
        Expr::If(condition, if_block, else_block) => match else_block {
            Some(box_else) => format!(
                "if {} {} else {}",
                expression_to_string(condition),
                statement_to_string(if_block),
                statement_to_string(box_else)
            ),
            None => format!(
                "if {} {}",
                expression_to_string(condition),
                statement_to_string(if_block)
            ),
        },
        Expr::FunctionLiteral(parameters, body) => {
            let mut output = String::new();
            for (index, ident) in parameters.iter().enumerate() {
                output += &ident;
                if parameters.len() - index != 1 {
                    output += ", ";
                }
            }
            format!("fn ({}) {}", output, statement_to_string(body))
        }
        Expr::CallExpression {
            function: boxed_func,
            arguments: args,
        } => {
            let mut arg_string = String::new();
            for (index, expr) in args.iter().enumerate() {
                arg_string.push_str(&expression_to_string(expr));
                if args.len() - (index + 1) > 0 {
                    println!("len {:?} index {}", args.len(), index);
                    arg_string.push_str(", ");
                }
            }

            format!("{}({})", expression_to_string(boxed_func), arg_string)
        }
        Expr::None => "none".to_string(),
    }
}

pub type Identifier = String;
