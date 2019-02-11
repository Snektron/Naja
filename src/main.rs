#[macro_use]
extern crate lazy_static;

pub mod ast;
pub mod parser;
pub mod runtime;

use std::fs::File;
use std::env;
use std::io::Read;

fn main() -> Result<(), std::io::Error> {
    let filename = env::args().nth(1);
    if let None = filename {
        eprintln!("Expected file name");
        return Ok(());
    }

    let mut contents = String::new();
    let mut file = File::open(filename.unwrap())?;
    file.read_to_string(&mut contents)?;

    let mut parser = parser::Parser::new(&contents);

    let ast = match parser.program() {
        Ok(ast) => ast,
        Err(err) => {
            eprintln!("Parse Error: {}", err);
            return Ok(());
        }
    };

    let mut rt = runtime::Runtime::new();
    let result = rt.execute(&ast);

    match result {
        Err(err) => eprintln!("Runtime Error: {}", err),
        Ok(None) => println!("No return value"),
        Ok(Some(val)) => println!("{:?}", val),
    }

    Ok(())
}
