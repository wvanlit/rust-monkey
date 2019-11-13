#[derive(Debug, PartialEq)]
pub struct Token {
    pub token_type: TokenType,
    pub literal: String,
}

impl Token {
    pub fn new(token_type: TokenType, literal: String) -> Token {
        Token {
            token_type: token_type,
            literal: literal,
        }
    }
}

pub fn lookup_identifier(ident: &str) -> TokenType {
    match ident {
        // General Keywords
        "let" => TokenType::LET,
        "fn" => TokenType::FUNCTION,
        "return" => TokenType::RETURN,

        // Conditional Statement Keywords
        "if" => TokenType::IF,
        "else" => TokenType::ELSE,

        // Boolean Keywords
        "true" => TokenType::TRUE,
        "false" => TokenType::FALSE,

        // Normal identifier
        _ => TokenType::IDENT,
    }
}

#[allow(non_camel_case_types)]
#[derive(Debug, PartialEq)]
pub enum TokenType {
    ILLEGAL,
    EOF,

    // Identifiers
    IDENT, // Add, foobar, x, y, etc
    INT,   // Integers such as 1, 33 or 89

    // Operators
    ASSIGN,   // '='
    PLUS,     // '+'
    MINUS,    // '-'
    BANG,     // '!'
    ASTERISK, // '*'
    SLASH,    // '/'

    LT, // '<'
    GT, // '>'

    EQ,     // '=='
    NOT_EQ, // '!='

    // Delimiters
    COMMA,     // ','
    SEMICOLON, // ';'

    LPAREN, // '('
    RPAREN, // ')'
    LBRACE, // '{'
    RBRACE, // '}'

    // General Keywords
    FUNCTION, // Function declaration
    LET,      // Variable declaration
    RETURN,   // return declaration

    // Conditional Statement Keywords
    IF,   // If statement
    ELSE, // Else statement

    // Boolean Keywords
    TRUE,  // true boolean
    FALSE, // false boolean
}