mod evaluate;
mod lexer;
mod parser;
mod repl;
mod tokens;

fn main() {
    repl::repl::start();
}
