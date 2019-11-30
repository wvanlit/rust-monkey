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
    CALL,
    // 'func(x)'
    INDEX,
    // 'arr[x]'
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
        TokenType::LPAREN => Precedence::CALL,
        TokenType::LBRACKET => Precedence::INDEX,
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

        // Parse Expression
        self.next_token();

        let value = match self.parse_expression(&mut Precedence::LOWEST) {
            Some(expr) => expr,
            None => ast::Expr::None,
        };

        while (*self.cur_token).token_type != TokenType::SEMICOLON {
            self.next_token();
        }

        Some(ast::Statement::Let(cur_token.literal.clone(), value))
    }

    fn parse_return_statement(&mut self) -> Option<ast::Statement> {
        self.next_token();

        let return_value = match self.parse_expression(&mut Precedence::LOWEST) {
            Some(expr) => expr,
            None => ast::Expr::None,
        };

        while (*self.cur_token).token_type != TokenType::SEMICOLON {
            self.next_token();
        }

        Some(ast::Statement::Return(return_value))
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
            TokenType::IF => self.parse_if_expression(),
            TokenType::FUNCTION => self.parse_function_literal(),
            TokenType::STRING => self.parse_string_literal(),
            TokenType::LBRACKET => self.parse_array_literal(),
            TokenType::LBRACE => self.parse_hash_literal(),
            _ => ast::Expr::None,
        }
    }

    fn parse_infix(
        &mut self,
        left: ast::Expr,
        token_type: TokenType,
    ) -> Result<ast::Expr, ast::Expr> {
        match token_type {
            TokenType::LPAREN => self.parse_call_expression(left),
            TokenType::LBRACKET => self.parse_index_expression(left),
            _ => self.parse_infix_expression(left),
        }
    }

    fn parse_expression(&mut self, priority: &mut Precedence) -> Option<ast::Expr> {
        let prefix_expr = self.parse_prefix((*self.cur_token).token_type);
        let mut left_expr = match prefix_expr {
            ast::Expr::None => {
                self.errors.push(Err(format!(
                    "No Prefix found for: {:?}",
                    (*self.cur_token).token_type
                )));
                return None;
            }
            _ => prefix_expr,
        };

        while !self.peek_token_is(TokenType::SEMICOLON)
        	// Check if the current precedence still doesn't overule the next one
            && priority < &mut self.peek_precedence()
            // Check if INFIX operator (has a precedence)
            && get_token_precedence((*self.peek_token).token_type) != Precedence::LOWEST
        {
            self.next_token();
            match self.parse_infix(left_expr, (*self.cur_token).token_type) {
                Ok(result) => {
                    left_expr = result;
                }
                Err(err) => return Some(err),
            }
        }

        Some(left_expr)
    }

    fn parse_identifier(&mut self) -> ast::Expr {
        let cur_token_box = mem::replace(
            &mut self.cur_token,
            Box::new(Token::new(TokenType::ILLEGAL, "".to_string())),
        );
        let cur_token = *cur_token_box;

        ast::Expr::Identifier(cur_token.literal.clone())
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

    fn parse_boolean(&mut self) -> ast::Expr {
        ast::Expr::Bool(self.cur_token_is(TokenType::TRUE))
    }

    fn parse_grouped_expression(&mut self) -> ast::Expr {
        self.next_token();

        let exp = self.parse_expression(&mut Precedence::LOWEST);

        if !self.expect_peek(TokenType::RPAREN) {
            self.errors
                .push(Err("Expected RPAREN to close LPAREN".to_string()));
            return ast::Expr::None;
        }

        match exp {
            Some(expr) => expr,
            None => ast::Expr::None,
        }
    }

    fn parse_block_statement(&mut self) -> ast::Statement {
        let mut statements: Vec<ast::Statement> = vec![];

        self.next_token();

        while !self.cur_token_is(TokenType::RBRACE) {
            match self.parse_statement() {
                Some(statement) => statements.push(statement),
                None => (),
            }
            self.next_token();
        }

        ast::Statement::BlockStatement(statements)
    }

    fn parse_if_expression(&mut self) -> ast::Expr {
        if !self.expect_peek(TokenType::LPAREN) {
            self.errors
                .push(Err("Expected LPAREN after IF!".to_string()));
            return ast::Expr::None;
        }

        self.next_token();
        // Parse Condition
        let condition = match self.parse_expression(&mut Precedence::LOWEST) {
            Some(expr) => Box::new(expr),
            None => Box::new(ast::Expr::None),
        };

        if !self.expect_peek(TokenType::RPAREN) {
            self.errors
                .push(Err("Expected RPAREN after IF condition!".to_string()));
            return ast::Expr::None;
        }
        // Parse Consequence (if then)
        if !self.expect_peek(TokenType::LBRACE) {
            self.errors.push(Err(
                "Expected BlockStatement after IF condition!".to_string()
            ));
            return ast::Expr::None;
        }

        let consequence = Box::new(self.parse_block_statement());

        // Parse Alternative (else)
        let alternative = if self.peek_token_is(TokenType::ELSE) {
            self.next_token();
            if !self.expect_peek(TokenType::LBRACE) {
                None
            } else {
                Some(Box::new(self.parse_block_statement()))
            }
        } else {
            None
        };

        ast::Expr::If(condition, consequence, alternative)
    }

    fn parse_function_parameters(&mut self) -> Vec<ast::Identifier> {
        let mut identifiers: Vec<ast::Identifier> = vec![];

        if self.peek_token_is(TokenType::RPAREN) {
            self.next_token();
            return identifiers;
        }

        self.next_token();

        let mut ident = self.cur_token.literal.clone();

        identifiers.push(ident);

        while self.peek_token_is(TokenType::COMMA) {
            self.next_token();
            self.next_token();

            ident = self.cur_token.literal.clone();

            identifiers.push(ident);
        }

        if !self.expect_peek(TokenType::RPAREN) {
            self.errors
                .push(Err("Expected RPAREN after parsing parameters!".to_string()));
        }

        identifiers
    }

    fn parse_function_literal(&mut self) -> ast::Expr {
        if !self.expect_peek(TokenType::LPAREN) {
            self.errors
                .push(Err("Expected LPAREN after FUNCTION!".to_string()));
            return ast::Expr::None;
        }

        let parameters = self.parse_function_parameters();

        if !self.expect_peek(TokenType::LBRACE) {
            self.errors
                .push(Err("Expected LBRACE after FUNCTION PARAMETERS!".to_string()));
            return ast::Expr::None;
        }

        let body = self.parse_block_statement();

        ast::Expr::FunctionLiteral(parameters, Box::new(body))
    }

    fn parse_string_literal(&mut self) -> ast::Expr {
        ast::Expr::String(self.cur_token.literal.clone())
    }

    fn parse_array_literal(&mut self) -> ast::Expr {
        ast::Expr::ArrayLiteral(self.parse_expression_list(TokenType::RBRACKET))
    }

    fn parse_hash_literal(&mut self) -> ast::Expr {
        let mut keys = vec![];

        while !self.peek_token_is(TokenType::RBRACE) {
            self.next_token();
            let key = match self.parse_expression(&mut Precedence::LOWEST) {
                Some(expr) => expr,
                None => ast::Expr::None,
            };

            if !self.expect_peek(TokenType::COLON) {
                return ast::Expr::None;
            }

            self.next_token();

            let value = match self.parse_expression(&mut Precedence::LOWEST) {
                Some(expr) => expr,
                None => ast::Expr::None,
            };

            keys.push((key, value));

            if !self.peek_token_is(TokenType::RBRACE) && !self.expect_peek(TokenType::COMMA) {
                return ast::Expr::None;
            }
        }

        if !self.expect_peek(TokenType::RBRACE) {
            return ast::Expr::None;
        }

        ast::Expr::HashLiteral(keys)
    }

    fn parse_expression_list(&mut self, until: TokenType) -> Vec<ast::Expr> {
        let mut args: Vec<ast::Expr> = vec![];

        if self.peek_token_is(until) {
            self.next_token();
            return args;
        }

        self.next_token();
        match self.parse_expression(&mut Precedence::LOWEST) {
            Some(expr) => args.push(expr),
            None => (),
        }

        while self.peek_token_is(TokenType::COMMA) {
            self.next_token();
            self.next_token();
            match self.parse_expression(&mut Precedence::LOWEST) {
                Some(expr) => args.push(expr),
                None => (),
            }
        }

        if !self.expect_peek(until) {
            self.errors.push(Err(
                "Expected RPAREN after parsing call arguments!".to_string()
            ));
        }

        args
    }

    fn parse_prefix_expression(&mut self) -> ast::Expr {
        let operator = self.cur_token.literal.clone();

        self.next_token();

        let right = match self.parse_expression(&mut Precedence::PREFIX) {
            Some(expr) => expr,
            None => {
                self.errors.push(Err(format!(
                    "No expression to the right of prefix {}",
                    operator
                )));
                ast::Expr::None
            }
        };
        ast::Expr::Prefix(operator.to_string(), Box::new(right))
    }

    fn parse_call_expression(&mut self, function: ast::Expr) -> Result<ast::Expr, ast::Expr> {
        Ok(ast::Expr::CallExpression {
            function: Box::new(function),
            arguments: self.parse_expression_list(TokenType::RPAREN),
        })
    }

    fn parse_index_expression(&mut self, left: ast::Expr) -> Result<ast::Expr, ast::Expr> {
        self.next_token();
        let right = match self.parse_expression(&mut Precedence::LOWEST) {
            Some(expr) => expr,
            None => return Err(ast::Expr::None),
        };

        if !self.expect_peek(TokenType::RBRACKET) {
            return Err(ast::Expr::None);
        }

        Ok(ast::Expr::Index(Box::new(left), Box::new(right)))
    }

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
                assert_eq!(*identifier, expected_identifier.to_string());
            }
            _ => assert!(false),
        };
    }

    #[test]
    fn test_let_statement_expressions() {
        let input = "
        let x = 5;
        let y = true;
        let foo = y;
        ";

        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);

        let mut program = parser.parse_program();

        // Check for errors
        for err in parser.errors.iter() {
            println!("{:?}", err.as_ref().unwrap_err());
        }
        assert_eq!(parser.errors.len(), 0);

        assert_eq!(program.statements.len(), 3);

        let x_is_5 = program.statements.remove(0);
        match x_is_5 {
            ast::Statement::Let(identifier, expr) => {
                assert_eq!(identifier, "x".to_string());
                test_integer_literal(&expr, 5);
            }
            _ => assert!(false),
        };

        let y_is_true = program.statements.remove(0);
        match y_is_true {
            ast::Statement::Let(identifier, expr) => {
                assert_eq!(identifier, "y".to_string());
                test_bool(&expr, true);
            }
            _ => assert!(false),
        };

        let foo_is_y = program.statements.remove(0);
        match foo_is_y {
            ast::Statement::Let(identifier, expr) => {
                assert_eq!(identifier, "foo".to_string());
                match expr {
                    ast::Expr::Identifier(ident) => assert_eq!(ident, "y".to_string()),
                    _ => assert!(false),
                }
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
                    assert_eq!(identifier, "foobar".to_string());
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

    fn test_bool(expr: &ast::Expr, expected: bool) {
        match expr {
            ast::Expr::Bool(value) => assert_eq!(*value, expected),
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

    fn ident(value: &str) -> ast::Expr {
        ast::Expr::Identifier(value.to_string())
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
                ast::Statement::Expr(expr) => {
                    test_infix_expression(&expr, &test.1, test.2, &test.3)
                }
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
            ("3 + 4; -5 * 5", "(3 + 4)\n((-5) * 5)"),
            ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
            ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
            ("true", "true"),
            ("false", "false"),
            ("3 > 5 == false", "((3 > 5) == false)"),
            ("3 < 5 != true", "((3 < 5) != true)"),
            ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
            ("(5 + 5) * 2", "((5 + 5) * 2)"),
            ("2 / (5 + 5)", "(2 / (5 + 5))"),
            ("-(5 + 5)", "(-(5 + 5))"),
            ("!(true == true)", "(!(true == true))"),
            ("a + add(b * c) + d", "((a + add((b * c))) + d)"),
            (
                "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
                "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
            ),
            (
                "add(a + b + c * d / f + g)",
                "add((((a + b) + ((c * d) / f)) + g))",
            ),
            (
                "a * [1, 2, 3, 4][b * c] * d",
                "((a * ([1, 2, 3, 4][(b * c)])) * d)",
            ),
            (
                "add(a * b[2], b[1], 2 * [1, 2][1])",
                "add((a * (b[2])), (b[1]), (2 * ([1, 2][1])))",
            ),
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
                assert_eq!(ident, expected);
            }
            _ => assert!(false),
        }
    }

    fn test_expression(expr: &ast::Expr, expected: &ast::Expr) {
        match expr {
            ast::Expr::Identifier(_) => match expected {
                ast::Expr::Identifier(ident) => test_identifier(expr, &ident),
                _ => assert!(false),
            },
            ast::Expr::IntegerLiteral(_) => match expected {
                ast::Expr::IntegerLiteral(i) => test_integer_literal(expr, *i),
                _ => assert!(false),
            },
            ast::Expr::Bool(val) => match expected {
                ast::Expr::Bool(val2) => assert_eq!(val, val2),
                _ => assert!(false),
            },
            _ => assert!(false),
        }
    }

    fn test_infix_expression(
        infix: &ast::Expr,
        left: &ast::Expr,
        operator: TokenType,
        right: &ast::Expr,
    ) {
        match infix {
            ast::Expr::Infix(op, box_left, box_right) => {
                // Test Left Expression
                test_expression(&box_left, left);
                // Test Right Expression
                test_expression(&box_right, right);
                // Test Operator
                assert_eq!(operator, *op);
            }
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
    fn test_if_expression() {
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
                    // Test the condition
                    test_infix_expression(&condition, &ident("x"), TokenType::LT, &ident("y"));

                    // Check the if block
                    match *if_block {
                        ast::Statement::BlockStatement(mut vec) => {
                            assert_eq!(vec.len(), 1);
                            match vec.pop().expect("Nothing in Vec") {
                                ast::Statement::Expr(expr) => {
                                    test_identifier(&expr, &"x".to_string())
                                }
                                _ => assert!(false),
                            }
                        }
                        _ => assert!(false),
                    };

                    // Make sure else is empty
                    match else_option {
                        Some(_) => assert!(false),
                        None => (),
                    }
                }
                _ => assert!(false),
            },
            _ => assert!(false),
        }
    }

    #[test]
    fn test_if_else_expression() {
        let input = "if (x < y) { x } else { y };";

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
                    // Test the condition
                    test_infix_expression(&condition, &ident("x"), TokenType::LT, &ident("y"));

                    // Check the if block
                    match *if_block {
                        ast::Statement::BlockStatement(mut vec) => {
                            assert_eq!(vec.len(), 1);
                            match vec.pop().expect("Nothing in Vec") {
                                ast::Statement::Expr(expr) => {
                                    test_identifier(&expr, &"x".to_string())
                                }
                                _ => assert!(false),
                            }
                        }
                        _ => assert!(false),
                    };

                    // Check Else Block
                    match else_option {
                        Some(else_block) => match *else_block {
                            ast::Statement::BlockStatement(mut vec) => {
                                assert_eq!(vec.len(), 1);
                                match vec.pop().expect("Nothing in Vec") {
                                    ast::Statement::Expr(expr) => {
                                        test_identifier(&expr, &"y".to_string())
                                    }
                                    _ => assert!(false),
                                }
                            }
                            _ => assert!(false),
                        },
                        None => assert!(false),
                    }
                }
                _ => assert!(false),
            },
            _ => assert!(false),
        }
    }

    #[test]
    fn test_function_literal_parsing() {
        let input = "fn(x, y) { x + y; }";

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
                ast::Expr::FunctionLiteral(mut parameters, body_box) => {
                    // Test Parameters
                    assert_eq!(parameters.len(), 2);

                    assert_eq!(parameters.remove(0), "x");
                    assert_eq!(parameters.remove(0), "y");

                    // Test Body
                    match *body_box {
                        ast::Statement::BlockStatement(mut statements) => {
                            assert_eq!(statements.len(), 1);
                            match statements.pop() {
                                Some(stmnt) => match stmnt {
                                    ast::Statement::Expr(expr) => test_infix_expression(
                                        &expr,
                                        &ident("x"),
                                        TokenType::PLUS,
                                        &ident("y"),
                                    ),
                                    _ => assert!(false),
                                },
                                None => assert!(false),
                            }
                        }
                        _ => assert!(false),
                    }
                }
                _ => assert!(false),
            },
            _ => assert!(false),
        }
    }

    #[test]
    fn test_call_expresion_parsing() {
        let input = "add(1, 2 * 3, 4 + 5)";

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
                ast::Expr::CallExpression {
                    function: boxed_func,
                    arguments: mut args,
                } => {
                    test_identifier(&boxed_func, &"add".to_string());

                    test_integer_literal(&args.remove(0), 1);
                    test_infix_expression(&args.remove(0), &int(2), TokenType::ASTERISK, &int(3));
                    test_infix_expression(&args.remove(0), &int(4), TokenType::PLUS, &int(5));
                }
                _ => assert!(false),
            },
            _ => assert!(false),
        }
    }

    #[test]
    fn test_string_literal_expression() {
        let input = r#" "hello world" "#;

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
                ast::Expr::String(s) => assert_eq!(s, "hello world".to_string()),
                _ => assert!(false, "Expression is {:?} instead of String", expr),
            },
            _ => assert!(false, "Statement is {:?} instead of Expr", statement),
        }
    }

    #[test]
    fn test_array_literal_parsing() {
        let input = "[1, 2 * 2, 3 + 3]";

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
                ast::Expr::ArrayLiteral(mut elements) => {
                    assert_eq!(elements.len(), 3);
                    test_infix_expression(&elements.remove(2), &int(3), TokenType::PLUS, &int(3));
                    test_infix_expression(
                        &elements.remove(1),
                        &int(2),
                        TokenType::ASTERISK,
                        &int(2),
                    );
                    test_integer_literal(&elements.remove(0), 1);
                }
                _ => assert!(false, "Expression is {:?} instead of Array", expr),
            },
            _ => assert!(false, "Statement is {:?} instead of Expr", statement),
        }
    }

    #[test]
    fn test_array_index_parsing() {
        let input = "arr[1 + 1]";

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
                ast::Expr::Index(left, index) => {
                    test_identifier(&left, &"arr".to_string());
                    test_infix_expression(&index, &int(1), TokenType::PLUS, &int(1));
                }
                _ => assert!(false, "Expression is {:?} instead of Index", expr),
            },
            _ => assert!(false, "Statement is {:?} instead of Expr", statement),
        }
    }

    fn string(s: &str) -> ast::Expr {
        ast::Expr::String(s.to_string())
    }

    #[test]
    fn test_hash_literal_parsing_string_keys() {
        let input = r#"{"one":1, true:2, 3:3}"#;

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
                ast::Expr::HashLiteral(keys) => {
                    assert_eq!(keys.len(), 3);

                    assert_eq!(keys[0].0, string("one"));
                    assert_eq!(keys[0].1, int(1));

                    assert_eq!(keys[1].0, bool(true));
                    assert_eq!(keys[1].1, int(2));

                    assert_eq!(keys[2].0, int(3));
                    assert_eq!(keys[2].1, int(3));
                }
                _ => assert!(false, "Expression is {:?} instead of HashLiteral", expr),
            },
            _ => assert!(false, "Statement is {:?} instead of Expr", statement),
        }
    }

    #[test]
    fn test_hash_literal_parsing_empty() {
        let input = r#"{}"#;

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
                ast::Expr::HashLiteral(keys) => {
                    assert_eq!(keys.len(), 0);
                }
                _ => assert!(false, "Expression is {:?} instead of HashLiteral", expr),
            },
            _ => assert!(false, "Statement is {:?} instead of Expr", statement),
        }
    }
}
