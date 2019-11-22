use crate::lexer::lexer::Lexer;
use crate::parser::ast;
use crate::tokens::tokens::{Token, TokenType};
use std::mem;

#[derive(Debug, PartialEq, PartialOrd)]
pub enum Precedence {
    LOWEST,
    EQUALS,
    // '==' or '!='
    LESSGREATER,
    // '<' or '>'
    SUM,
    // '+' or '-'
    PRODUCT,
    // '*' or '/'
    PREFIX,
    // '-x' or '!x'
    CALL, // 'func(x)'
}

fn get_token_precedence(token_type: TokenType) -> Precedence {
    match token_type {
        TokenType::EQ => Precedence::EQUALS,
        TokenType::NOT_EQ => Precedence::EQUALS,
        TokenType::LT => Precedence::LESSGREATER,
        TokenType::GT => Precedence::LESSGREATER,
        TokenType::PLUS => Precedence::SUM,
        TokenType::MINUS => Precedence::SUM,
        TokenType::SLASH => Precedence::PRODUCT,
        TokenType::ASTERISK => Precedence::PRODUCT,
        _ => Precedence::LOWEST,
    }
}

pub struct Parser {
    lexer: Lexer,

    cur_token: Box<Token>,
    peek_token: Box<Token>,

    pub errors: Vec<Result<(), String>>,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Parser {
        let mut parser = Parser {
            lexer: lexer,

            cur_token: Box::new(Token::new(TokenType::ILLEGAL, "".to_string())),
            peek_token: Box::new(Token::new(TokenType::ILLEGAL, "".to_string())),

            errors: vec![],
        };

        // Setup Tokens
        parser.next_token();
        parser.next_token();

        parser
    }

    /*
     *	Token Manipulation
     */

    fn next_token(&mut self) {
        // Put peek into cur and put the next token into peek
        self.cur_token = mem::replace(&mut self.peek_token, Box::new(self.lexer.next_token()));
    }

    fn cur_token_is(&self, token_type: TokenType) -> bool {
        (*self.cur_token).token_type == token_type
    }

    fn peek_token_is(&self, token_type: TokenType) -> bool {
        (*self.peek_token).token_type == token_type
    }

    fn expect_peek(&mut self, token_type: TokenType) -> bool {
        if self.peek_token_is(token_type) {
            self.next_token();
            true
        } else {
            self.errors.push(Err(format!(
                "Token {:?} expected, instead got {:?}!",
                token_type,
                (*self.peek_token).token_type
            )));
            false
        }
    }

    fn curr_precedence(&self) -> Precedence {
        get_token_precedence((*self.cur_token).token_type)
    }

    fn peek_precedence(&self) -> Precedence {
        get_token_precedence((*self.peek_token).token_type)
    }
    /*
     *	Program Parsing
     */

    pub fn parse_program(&mut self) -> ast::Program {
        let mut program = ast::Program { statements: vec![] };

        loop {
            match (*self.cur_token).token_type {
                TokenType::EOF => break,
                _ => {
                    let statement_option = self.parse_statement();
                    match statement_option {
                        Some(statement) => program.statements.push(statement),
                        None => (),
                    }
                    self.next_token();
                }
            }
        }
        program
    }

    /*
     *	Statement Parsing
     */

    fn parse_statement(&mut self) -> Option<ast::Statement> {
        match (*self.cur_token).token_type {
            TokenType::LET => self.parse_let_statement(),
            TokenType::RETURN => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_let_statement(&mut self) -> Option<ast::Statement> {
        if !self.expect_peek(TokenType::IDENT) {
            return None;
        }

        let cur_token_box = mem::replace(
            &mut self.cur_token,
            Box::new(Token::new(TokenType::ILLEGAL, "".to_string())),
        );
        let cur_token = *cur_token_box;

        if !self.expect_peek(TokenType::ASSIGN) {
            return None;
        }

        // TODO parse Expression
        while (*self.cur_token).token_type != TokenType::SEMICOLON {
            self.next_token();
        }

        Some(ast::Statement::Let(
            ast::Identifier {
                value: cur_token.literal.clone(),
                token: cur_token,
            },
            ast::Expr::None,
        ))
    }

    fn parse_return_statement(&mut self) -> Option<ast::Statement> {
        self.next_token();

        // TODO parse Expression
        while (*self.cur_token).token_type != TokenType::SEMICOLON {
            self.next_token();
        }

        Some(ast::Statement::Return(ast::Expr::None))
    }

    fn parse_expression_statement(&mut self) -> Option<ast::Statement> {
        let expr = self.parse_expression(&mut Precedence::LOWEST);

        // Check for the optional semicolon
        if self.peek_token_is(TokenType::SEMICOLON) {
            self.next_token();
        }
        match expr {
            Some(expr) => Some(ast::Statement::Expr(expr)),
            None => None,
        }
    }

    /*
     *	Expression Parsing
     */

    fn parse_prefix(&mut self, token_type: TokenType) -> ast::Expr {
        match token_type {
            TokenType::IDENT => self.parse_identifier(),
            TokenType::INT => self.parse_integer_literal(),
            TokenType::BANG | TokenType::MINUS => self.parse_prefix_expression(),
            TokenType::TRUE | TokenType::FALSE => self.parse_boolean(),
            TokenType::LPAREN => self.parse_grouped_expression(),
            _ => ast::Expr::None
        }
    }

    fn parse_expression(&mut self, priority: &mut Precedence) -> Option<ast::Expr> {
        let prefix_expr = self.parse_prefix((*self.cur_token).token_type);
        let mut left_expr = match prefix_expr {
            ast::Expr::None => {
                self.errors.push(Err(format!("No Prefix found for: {:?}", (*self.cur_token).token_type)));
                return None;
            }
            _ => prefix_expr,
        };


        while !self.peek_token_is(TokenType::SEMICOLON)
        	// Check if the current precedence still doesn't overule the next one
            && priority < &mut self.peek_precedence()
            // Check if INFIX operator (has a precedence)
            && get_token_precedence((*self.peek_token).token_type) != Precedence::LOWEST{
            self.next_token();
            match self.parse_infix_expression(left_expr) {
                Ok(result) => {
                    left_expr = result;
                }
                Err(err) => return Some(err),
            }
        }

        Some(left_expr)
    }

    /*
     *	Prefix Parsing
     */

    fn parse_identifier(&mut self) -> ast::Expr {
        let cur_token_box = mem::replace(
            &mut self.cur_token,
            Box::new(Token::new(TokenType::ILLEGAL, "".to_string())),
        );
        let cur_token = *cur_token_box;

        ast::Expr::Identifier(ast::Identifier {
            value: cur_token.literal.clone(),
            token: cur_token,
        })
    }

    fn parse_integer_literal(&mut self) -> ast::Expr {
        let value = self.cur_token.literal.parse();
        match value {
            Ok(expr) => ast::Expr::IntegerLiteral(expr),
            Err(e) => {
                self.errors.push(Err(e.to_string()));
                ast::Expr::None
            }
        }
    }

    fn parse_boolean(&mut self) -> ast::Expr{
        ast::Expr::Bool(self.cur_token_is(TokenType::TRUE))
    }

    fn parse_grouped_expression(&mut self) -> ast::Expr {
        self.next_token();

        let exp = self.parse_expression(&mut Precedence::LOWEST);

        if !self.expect_peek(TokenType::RPAREN){
            self.errors.push(Err("Expected RPAREN ')'!".to_string()));
            return ast::Expr::None
        }

        match exp {
            Some(expr) => expr,
            None => ast::Expr::None,
        }
    }

    fn parse_prefix_expression(&mut self) -> ast::Expr {
        let operator = self.cur_token.literal.clone();

        self.next_token();

        let right = match self.parse_expression(&mut Precedence::PREFIX) {
            Some(expr) => expr,
            None => {
                self.errors.push(Err(format!("No expression to the right of prefix {}", operator)));
                ast::Expr::None
            }
        };
        ast::Expr::Prefix(operator.to_string(), Box::new(right))
    }

    /*
     *	Infix Parsing
     */

    fn parse_infix_expression(&mut self, left: ast::Expr) -> Result<ast::Expr, ast::Expr> {
        let mut precedence = self.curr_precedence();
        let operator = self.cur_token.token_type;

        self.next_token();
        let right = match self.parse_expression(&mut precedence) {
            Some(expr) => expr,
            None => ast::Expr::None,
        };

        Ok(ast::Expr::Infix(operator, Box::new(left), Box::new(right)))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_let_statement() {
        let input = "
		let x = 5;
		let y = 10;
		let foobar = 838383;
		";

        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        // Check for errors
        for err in parser.errors.iter() {
            println!("{:?}", err.as_ref().unwrap_err());
        }
        assert_eq!(parser.errors.len(), 0);

        assert_eq!(program.statements.len(), 3);

        let expected_identifier = ["x", "y", "foobar"];

        for (index, expected_name) in expected_identifier.iter().enumerate() {
            assert_let_statement(&program.statements[index], expected_name);
        }
    }

    fn assert_let_statement(statement: &ast::Statement, expected_identifier: &str) {
        match statement {
            ast::Statement::Let(identifier, _) => {
                assert_eq!(identifier.value, expected_identifier.to_string());
                assert_eq!(identifier.token.literal, expected_identifier.to_string());
            }
            _ => assert!(false),
        };
    }

    #[test]
    fn test_return_statement() {
        let input = "
    		return 5;
    		return 10;
    		return 993322;
    	";

        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        // Check for errors
        for err in parser.errors.iter() {
            println!("{:?}", err.as_ref().unwrap_err());
        }
        assert_eq!(parser.errors.len(), 0);

        assert_eq!(program.statements.len(), 3);

        //let expected_identifier = ["x", "y", "foobar"];

        for statement in program.statements.iter() {
            match statement {
                ast::Statement::Return(_) => (),
                _ => assert!(false),
            };
        }
    }

    #[test]
    fn test_identifier_expression() {
        let input = "foobar;";

        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);

        let mut program = parser.parse_program();

        // Check for errors
        for err in parser.errors.iter() {
            println!("{:?}", err.as_ref().unwrap_err());
        }

        assert_eq!(parser.errors.len(), 0);

        assert_eq!(program.statements.len(), 1);

        let statement = program.statements.pop().expect("Expected Statement!");
        match statement {
            ast::Statement::Expr(expr) => match expr {
                ast::Expr::Identifier(identifier) => {
                    assert_eq!(identifier.value, "foobar".to_string());
                    assert_eq!(identifier.token.literal, "foobar".to_string());
                }
                _ => assert!(false),
            },
            _ => assert!(false),
        }
    }

    #[test]
    fn test_integer_expression() {
        let input = "5;";

        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);

        let mut program = parser.parse_program();

        // Check for errors
        for err in parser.errors.iter() {
            println!("{:?}", err.as_ref().unwrap_err());
        }

        assert_eq!(parser.errors.len(), 0);

        assert_eq!(program.statements.len(), 1);

        let statement = program.statements.pop().expect("Expected Statement!");
        match statement {
            ast::Statement::Expr(expr) => match expr {
                ast::Expr::IntegerLiteral(value) => {
                    assert_eq!(value, 5);
                }
                _ => assert!(false),
            },
            _ => assert!(false),
        }
    }

    fn test_integer_literal(expr: &ast::Expr, expected: i32) {
        match expr {
            ast::Expr::IntegerLiteral(value) => assert_eq!(*value, expected),
            _ => assert!(false),
        }
    }

    #[test]
    fn test_parsing_prefix_expressions() {
        let input = vec![
            ("!5;", "!", int(5)),
            ("-15;", "-", int(15)),
            ("!true;", "!", bool(true)),
            ("!false;", "!", bool(false)),
        ];

        for test in input.iter() {
            let lexer = Lexer::new(test.0.to_string());
            let mut parser = Parser::new(lexer);

            let mut program = parser.parse_program();

            // Check for errors
            for err in parser.errors.iter() {
                println!("{:?}", err.as_ref().unwrap_err());
            }

            assert_eq!(parser.errors.len(), 0);

            assert_eq!(program.statements.len(), 1);

            let statement = program.statements.pop().expect("Expected Statement!");
            match statement {
                ast::Statement::Expr(expr) => match expr {
                    ast::Expr::Prefix(value, right) => {
                        assert_eq!(value, test.1);
                        test_expression(&right, &test.2);
                    }
                    _ => assert!(false),
                },
                _ => assert!(false),
            }
        }
    }

    fn int(i: i32) -> ast::Expr {
        ast::Expr::IntegerLiteral(i)
    }

    fn bool(b: bool) -> ast::Expr {
        ast::Expr::Bool(b)
    }

    fn ident(value: String) -> ast::Expr{
        ast::Expr::Ident(ast::Identifier{
            value: value,
            token: Token::new(TokenType::IDENT, value)
        })
    }

    #[test]
    fn test_parsing_infix_expressions() {
        let input = vec![
            ("5 + 5;", int(5), TokenType::PLUS, int(5)),
            ("5 - 5;", int(5), TokenType::MINUS, int(5)),
            ("5 * 5;", int(5), TokenType::ASTERISK, int(5)),
            ("5 / 5;", int(5), TokenType::SLASH, int(5)),
            ("5 > 5;", int(5), TokenType::GT, int(5)),
            ("5 < 5;", int(5), TokenType::LT, int(5)),
            ("5 == 5;", int(5), TokenType::EQ, int(5)),
            ("5 != 5;", int(5), TokenType::NOT_EQ, int(5)),
            ("true != false;", bool(true), TokenType::NOT_EQ, bool(false)),
            ("false == false;", bool(false), TokenType::EQ, bool(false)),
            ("true == true;", bool(true), TokenType::EQ, bool(true)),
        ];

        for test in input.iter() {
            let lexer = Lexer::new(test.0.to_string());
            let mut parser = Parser::new(lexer);

            let mut program = parser.parse_program();

            // Check for errors
            for err in parser.errors.iter() {
                println!("{:?}", err.as_ref().unwrap_err());
            }

            assert_eq!(parser.errors.len(), 0);

            assert_eq!(program.statements.len(), 1);

            let statement = program.statements.pop().expect("Expected Statement!");
            match statement {
                ast::Statement::Expr(expr) => test_infix_expression(&expr, &test.1, test.2, &test.3),
                _ => assert!(false),
            }
        }
    }

    #[test]
    fn test_operator_precedence() {
    	let input = vec![
    	("-a * b", "((-a) * b)"),
        ("!-a", "(!(-a))"),
        ("a + b + c", "((a + b) + c)"),
        ("a * b * c", "((a * b) * c)"),
        ("a * b / c", "((a * b) / c)"),
        ("a + b / c", "(a + (b / c))"),
        ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
        ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
        ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
        ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
        ("3 + 4 * 5 == 3 * 1 + 4 * 5", "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))"),
        ("3 + 4 * 5 == 3 * 1 + 4 * 5", "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))"),
    	("true", "true"),
        ("false", "false"),
        ("3 > 5 == false", "((3 > 5) == false)"),
        ("3 < 5 != true", "((3 < 5) != true)"),
        ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
        ("(5 + 5) * 2", "((5 + 5) * 2)"),
        ("2 / (5 + 5)", "(2 / (5 + 5))"),
        ("-(5 + 5)", "(-(5 + 5))"),
        ("!(true == true)", "(!(true == true))")
        ];

    	for test in input.iter() {
            let lexer = Lexer::new(test.0.to_string());
            let mut parser = Parser::new(lexer);

            let program = parser.parse_program();

            // Check for errors
            for err in parser.errors.iter() {
                println!("{:?}", err.as_ref().unwrap_err());
            }

            assert_eq!(parser.errors.len(), 0);

            assert_eq!(ast::program_to_string(&program), test.1);            
        }
    }

    fn test_identifier(expr: &ast::Expr, expected: &String) {
        match expr {
            ast::Expr::Identifier(ident) => {
                assert_eq!(ident.value, *expected); 
            },
            _ => assert!(false),
        }
    }

    fn test_expression(expr: &ast::Expr, expected: &ast::Expr){
        match expr {
            ast::Expr::Identifier(_) => {
                match expected {
                    ast::Expr::Identifier(ident) => test_identifier(expr, &ident.value),
                    _ => assert!(false),
                }
            },
            ast::Expr::IntegerLiteral(_) => {
                match expected {
                    ast::Expr::IntegerLiteral(i) => test_integer_literal(expr, *i),
                    _ => assert!(false),
                }
            }
            ast::Expr::Bool(val) => {
                match expected {
                    ast::Expr::Bool(val2) => assert_eq!(val, val2),
                    _ => assert!(false),
                }
            },
            _ => assert!(false),
        }
    }

    fn test_infix_expression(infix: &ast::Expr, left: &ast::Expr, operator: TokenType, right: &ast::Expr) {
        match infix {
            ast::Expr::Infix(op, box_left, box_right) => {
                // Test Left Expression
                test_expression(&box_left, left);
                // Test Right Expression
                test_expression(&box_right, right);
                // Test Operator
                assert_eq!(operator, *op);
            },
            _ => assert!(false),
        }
    }

    #[test]
    fn test_boolean_expression() {
        let input = "true;";

        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);

        let mut program = parser.parse_program();

        // Check for errors
        for err in parser.errors.iter() {
            println!("{:?}", err.as_ref().unwrap_err());
        }

        assert_eq!(parser.errors.len(), 0);

        assert_eq!(program.statements.len(), 1);

        let statement = program.statements.pop().expect("Expected Statement!");
        match statement {
            ast::Statement::Expr(expr) => match expr {
                ast::Expr::Bool(value) => {
                    assert_eq!(value, true);
                }
                _ => assert!(false),
            },
            _ => assert!(false),
        }
    }

    #[test]
    fn test_if_expression(){
        let input = "if (x < y) { x };";

        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);

        let mut program = parser.parse_program();

        // Check for errors
        for err in parser.errors.iter() {
            println!("{:?}", err.as_ref().unwrap_err());
        }

        assert_eq!(parser.errors.len(), 0);

        assert_eq!(program.statements.len(), 1);

        let statement = program.statements.pop().expect("Expected Statement!");
        match statement {
            ast::Statement::Expr(expr) => match expr {
                ast::Expr::If(condition, if_block, else_option) => {
                   test_infix_expression(condition, , TokenType::LT, )
                }
                _ => assert!(false),
            },
            _ => assert!(false),
        }
    }

}
