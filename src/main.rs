#[macro_use]
extern crate lazy_static;

mod ast;
mod parser;
mod runtime;

fn main() {
    let input = r#"
        c = 1 + 2
    "#;
    let mut parser = parser::Parser::new(input);

    let ast = match parser.program() {
        Ok(ast) => ast,
        Err(err) => {
            eprintln!("Parse Error: {}", err);
            return;
        }
    };

    let mut rt = runtime::Runtime::new();
    let result = rt.execute(&ast);
    if let Err(err) = result {
        eprintln!("Runtime Error: {}", err);
    }
}