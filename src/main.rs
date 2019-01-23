#[macro_use]
extern crate lazy_static;

mod parser;
mod ast;

fn main() {
    let input = r#"
        fn add_1(a, b) {
            return a + b
        }

        fn add_2(a, b) = a + b
    "#;
    let mut parser = parser::Parser::new(input);
    match parser.program() {
        Ok(ast) => println!("{:#?}", ast),
        Err(err) => eprintln!("Error: {}", err)
    }
}
