use crate::lexer::lexer::Lexer;
use crate::parser::parser::Parser;
use crate::parser::ast::{program_to_string};
use std::io::{self};

const PROMPT: &str = "$> ";

pub fn start() {
    println!("Welcome to the Monkey REPL!");

    // REPL Loop
    loop {
        // Print prompt and flush to write it to console
        print!("{}", PROMPT);
        io::Write::flush(&mut io::stdout()).expect("flush failed!");

        // Scan Input line
        let mut buffer = String::new();
        match io::stdin().read_line(&mut buffer) {
            Err(error) => {
                println!("error: {}", error);
                return;
            }
            Ok(_) => (),
        }

        // Create Lexer
        let lexer = Lexer::new(buffer);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        if parser.errors.len() > 0 {
            for err in parser.errors.iter() {
                println!("{:?}", err.as_ref().unwrap_err());
            }
            continue;
        }

        println!("{}", program_to_string(&program));

    }
}
