#[macro_use]
extern crate lazy_static;

mod parser;
mod ast;

fn main() {
    let input = r#"
        a + 1 = b
    "#;
    let mut parser = parser::Parser::new(input);
    match parser.program() {
        Ok(ast) => println!("{:#?}", ast),
        Err(err) => eprintln!("Error: {}", err)
    }
}
