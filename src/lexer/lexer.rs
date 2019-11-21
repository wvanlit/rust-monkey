use crate::tokens::tokens::{lookup_identifier, Token, TokenType};

#[derive(Debug)]
pub struct Lexer {
    input: String,
    input_chars: Vec<char>, // Char Vector to allow getting chars at certain indices
    position: usize,        // Current position in input (positions to current_char)
    read_position: usize,   // Current reading position in input (positions to after current_char)
    current_char: char,     // Char that is currently being examined
}

impl Lexer {
    pub fn new(input: String) -> Lexer {
        let mut lexer = Lexer {
            input: input.clone(),
            input_chars: input.chars().collect(),
            position: 0,
            read_position: 0,
            current_char: '\0',
        };
        lexer.read_char();
        lexer
    }

    fn peek_char(&self) -> char {
        if self.read_position >= self.input.len() {
            '\0'
        } else {
            self.input_chars[self.read_position]
        }
    }

    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.current_char = '\0';
        } else {
            self.current_char = self.input_chars[self.read_position];
        }

        self.position = self.read_position;
        self.read_position += 1;
    }

    fn read_identifier(&mut self) -> &str {
        let position = self.position;
        while is_letter(self.current_char) {
            self.read_char();
        }
        &self.input[position..self.position]
    }

    fn read_number(&mut self) -> &str {
        let position = self.position;
        while is_digit(self.current_char) {
            self.read_char();
        }
        &self.input[position..self.position]
    }

    fn skip_whitespace(&mut self) {
        while self.current_char.is_ascii_whitespace() {
            self.read_char();
        }
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();
        let token = match self.current_char {
            // Operators
            '=' => {
                // Check if '==' else '='
                if self.peek_char() == '=' {
                    let ch = self.current_char;
                    self.read_char();
                    Token::new(
                        TokenType::EQ,
                        ch.to_string() + &self.current_char.to_string(),
                    )
                } else {
                    Token::new(TokenType::ASSIGN, self.current_char.to_string())
                }
            }

            '!' => {
                // Check if '!=' else '!'
                if self.peek_char() == '=' {
                    let ch = self.current_char;
                    self.read_char();
                    Token::new(
                        TokenType::NOT_EQ,
                        ch.to_string() + &self.current_char.to_string(),
                    )
                } else {
                    Token::new(TokenType::BANG, self.current_char.to_string())
                }
            }

            // Mathematical Operators
            '+' => Token::new(TokenType::PLUS, self.current_char.to_string()),
            '-' => Token::new(TokenType::MINUS, self.current_char.to_string()),

            '*' => Token::new(TokenType::ASTERISK, self.current_char.to_string()),
            '/' => Token::new(TokenType::SLASH, self.current_char.to_string()),

            '<' => Token::new(TokenType::LT, self.current_char.to_string()),
            '>' => Token::new(TokenType::GT, self.current_char.to_string()),

            // Delimiters
            ';' => Token::new(TokenType::SEMICOLON, self.current_char.to_string()),
            ',' => Token::new(TokenType::COMMA, self.current_char.to_string()),

            '{' => Token::new(TokenType::LBRACE, self.current_char.to_string()),
            '}' => Token::new(TokenType::RBRACE, self.current_char.to_string()),
            '(' => Token::new(TokenType::LPAREN, self.current_char.to_string()),
            ')' => Token::new(TokenType::RPAREN, self.current_char.to_string()),

            // End of File delimiter
            '\0' => Token::new(TokenType::EOF, "".to_string()),
            _ => {
                if is_letter(self.current_char) {
                    let literal = self.read_identifier();
                    return Token::new(lookup_identifier(literal), literal.to_string());
                } else if is_digit(self.current_char) {
                    return Token::new(TokenType::INT, self.read_number().to_string());
                } else {
                    Token::new(TokenType::ILLEGAL, self.current_char.to_string())
                }
            }
        };
        self.read_char();
        token
    }
}

fn is_letter(ch: char) -> bool {
    ch.is_alphabetic() || ch == '_'
}

fn is_digit(ch: char) -> bool {
    ch.is_digit(10)
}

#[cfg(test)]
mod tests {
    use super::Lexer;
    use crate::tokens::tokens::{Token, TokenType};

    #[test]
    fn next_token() {
        let input = "=+(){},;";

        let expected = [
            Token {
                token_type: TokenType::ASSIGN,
                literal: "=".to_string(),
            },
            Token {
                token_type: TokenType::PLUS,
                literal: "+".to_string(),
            },
            Token {
                token_type: TokenType::LPAREN,
                literal: "(".to_string(),
            },
            Token {
                token_type: TokenType::RPAREN,
                literal: ")".to_string(),
            },
            Token {
                token_type: TokenType::LBRACE,
                literal: "{".to_string(),
            },
            Token {
                token_type: TokenType::RBRACE,
                literal: "}".to_string(),
            },
            Token {
                token_type: TokenType::COMMA,
                literal: ",".to_string(),
            },
            Token {
                token_type: TokenType::SEMICOLON,
                literal: ";".to_string(),
            },
        ];

        let mut lexer = Lexer::new(input.to_string());

        for token in expected.iter() {
            let lexed_token = lexer.next_token();
            assert_eq!(lexed_token, *token);
        }
    }

    #[test]
    fn number_parsing() {
        let input = "let x = 11;";

        let expected = [
            Token::new(TokenType::LET, "let".to_string()),
            Token::new(TokenType::IDENT, "x".to_string()),
            Token::new(TokenType::ASSIGN, "=".to_string()),
            Token::new(TokenType::INT, "11".to_string()),
            Token::new(TokenType::SEMICOLON, ";".to_string()),
            Token::new(TokenType::EOF, "".to_string()),
        ];

        let mut lexer = Lexer::new(input.to_string());

        for token in expected.iter() {
            let lexed_token = lexer.next_token();
            assert_eq!(lexed_token, *token);
        }
    }

    #[test]
    fn simple_code() {
        let input = "
        let five = 5;
        let ten = 10;

        let add = fn(x, y){
            x+y;
        };
        
        let result = add(five, ten);
        ";

        let expected = [
            Token::new(TokenType::LET, "let".to_string()),
            Token::new(TokenType::IDENT, "five".to_string()),
            Token::new(TokenType::ASSIGN, "=".to_string()),
            Token::new(TokenType::INT, "5".to_string()),
            Token::new(TokenType::SEMICOLON, ";".to_string()),
            Token::new(TokenType::LET, "let".to_string()),
            Token::new(TokenType::IDENT, "ten".to_string()),
            Token::new(TokenType::ASSIGN, "=".to_string()),
            Token::new(TokenType::INT, "10".to_string()),
            Token::new(TokenType::SEMICOLON, ";".to_string()),
            Token::new(TokenType::LET, "let".to_string()),
            Token::new(TokenType::IDENT, "add".to_string()),
            Token::new(TokenType::ASSIGN, "=".to_string()),
            Token::new(TokenType::FUNCTION, "fn".to_string()),
            Token::new(TokenType::LPAREN, "(".to_string()),
            Token::new(TokenType::IDENT, "x".to_string()),
            Token::new(TokenType::COMMA, ",".to_string()),
            Token::new(TokenType::IDENT, "y".to_string()),
            Token::new(TokenType::RPAREN, ")".to_string()),
            Token::new(TokenType::LBRACE, "{".to_string()),
            Token::new(TokenType::IDENT, "x".to_string()),
            Token::new(TokenType::PLUS, "+".to_string()),
            Token::new(TokenType::IDENT, "y".to_string()),
            Token::new(TokenType::SEMICOLON, ";".to_string()),
            Token::new(TokenType::RBRACE, "}".to_string()),
            Token::new(TokenType::SEMICOLON, ";".to_string()),
            Token::new(TokenType::LET, "let".to_string()),
            Token::new(TokenType::IDENT, "result".to_string()),
            Token::new(TokenType::ASSIGN, "=".to_string()),
            Token::new(TokenType::IDENT, "add".to_string()),
            Token::new(TokenType::LPAREN, "(".to_string()),
            Token::new(TokenType::IDENT, "five".to_string()),
            Token::new(TokenType::COMMA, ",".to_string()),
            Token::new(TokenType::IDENT, "ten".to_string()),
            Token::new(TokenType::RPAREN, ")".to_string()),
            Token::new(TokenType::SEMICOLON, ";".to_string()),
            Token::new(TokenType::EOF, "".to_string()),
        ];

        let mut lexer = Lexer::new(input.to_string());

        for token in expected.iter() {
            let lexed_token = lexer.next_token();
            assert_eq!(lexed_token, *token);
        }
    }

    #[test]
    fn extended_tokens() {
        let input = "
            !-/*5;
            5 < 10 > 5;
        ";

        let expected = [
            Token::new(TokenType::BANG, "!".to_string()),
            Token::new(TokenType::MINUS, "-".to_string()),
            Token::new(TokenType::SLASH, "/".to_string()),
            Token::new(TokenType::ASTERISK, "*".to_string()),
            Token::new(TokenType::INT, "5".to_string()),
            Token::new(TokenType::SEMICOLON, ";".to_string()),
            Token::new(TokenType::INT, "5".to_string()),
            Token::new(TokenType::LT, "<".to_string()),
            Token::new(TokenType::INT, "10".to_string()),
            Token::new(TokenType::GT, ">".to_string()),
            Token::new(TokenType::INT, "5".to_string()),
            Token::new(TokenType::SEMICOLON, ";".to_string()),
            Token::new(TokenType::EOF, "".to_string()),
        ];

        let mut lexer = Lexer::new(input.to_string());

        for token in expected.iter() {
            let lexed_token = lexer.next_token();
            assert_eq!(lexed_token, *token);
        }
    }

    #[test]
    fn if_else_tokens() {
        let input = "
            if (5 < 10) {
                return true;
            } else {
                return false;
            }
        ";

        let expected = [
            Token::new(TokenType::IF, "if".to_string()),
            Token::new(TokenType::LPAREN, "(".to_string()),
            Token::new(TokenType::INT, "5".to_string()),
            Token::new(TokenType::LT, "<".to_string()),
            Token::new(TokenType::INT, "10".to_string()),
            Token::new(TokenType::RPAREN, ")".to_string()),
            Token::new(TokenType::LBRACE, "{".to_string()),
            Token::new(TokenType::RETURN, "return".to_string()),
            Token::new(TokenType::TRUE, "true".to_string()),
            Token::new(TokenType::SEMICOLON, ";".to_string()),
            Token::new(TokenType::RBRACE, "}".to_string()),
            Token::new(TokenType::ELSE, "else".to_string()),
            Token::new(TokenType::LBRACE, "{".to_string()),
            Token::new(TokenType::RETURN, "return".to_string()),
            Token::new(TokenType::FALSE, "false".to_string()),
            Token::new(TokenType::SEMICOLON, ";".to_string()),
            Token::new(TokenType::RBRACE, "}".to_string()),
            Token::new(TokenType::EOF, "".to_string()),
        ];

        let mut lexer = Lexer::new(input.to_string());

        for token in expected.iter() {
            let lexed_token = lexer.next_token();
            assert_eq!(lexed_token, *token);
        }
    }

    #[test]
    fn equals_not_equals() {
        let input = "
            10 == 10;
            10 != 9;
        ";

        let expected = [
            Token::new(TokenType::INT, "10".to_string()),
            Token::new(TokenType::EQ, "==".to_string()),
            Token::new(TokenType::INT, "10".to_string()),
            Token::new(TokenType::SEMICOLON, ";".to_string()),
            Token::new(TokenType::INT, "10".to_string()),
            Token::new(TokenType::NOT_EQ, "!=".to_string()),
            Token::new(TokenType::INT, "9".to_string()),
            Token::new(TokenType::SEMICOLON, ";".to_string()),
            Token::new(TokenType::EOF, "".to_string()),
        ];

        let mut lexer = Lexer::new(input.to_string());

        for token in expected.iter() {
            let lexed_token = lexer.next_token();
            assert_eq!(lexed_token, *token);
        }
    }
}
