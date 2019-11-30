use std::fmt;

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
#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum TokenType {
    ILLEGAL,
    EOF,

    // Identifiers
    IDENT, // Add, foobar, x, y, etc
    INT,   // Integers such as 1, 33 or 89
    STRING,

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
    COLON, // ':'

    LPAREN, // '('
    RPAREN, // ')'
    LBRACE, // '{'
    RBRACE, // '}'
    LBRACKET, // '['
    RBRACKET, // ']'

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

impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TokenType::ASTERISK => write!(f, "*"),
            TokenType::SLASH => write!(f, "/"),
            TokenType::PLUS => write!(f, "+"),
            TokenType::MINUS => write!(f, "-"),
            TokenType::GT => write!(f, ">"),
            TokenType::LT => write!(f, "<"),
            TokenType::EQ => write!(f, "=="),
            TokenType::NOT_EQ => write!(f, "!="),
            _ => write!(f, "{:?}", self),
        }
    }
}
