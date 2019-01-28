#[macro_use]
extern crate lazy_static;

mod ast;
mod parser;
mod runtime;

fn main() {
    let input = r#"
        fn test(x) = x * 2

        return test(2)
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

    use runtime::value::Value;

    match result {
        Err(err) => eprintln!("Runtime Error: {}", err),
        Ok(None) => println!("No return value"),
        Ok(Some(Value::Null)) => println!("Null"),
        Ok(Some(Value::Integer(x))) => println!("Integer {}", x),
        Ok(Some(Value::Float(x))) => println!("Float {}", x),
        Ok(_) => println!("Other value")
    }
}