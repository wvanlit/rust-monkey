mod lexer;
mod parser;
mod repl;
mod tokens;
mod evaluate;

fn main() {
    repl::repl::start();
}
