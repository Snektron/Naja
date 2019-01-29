#[macro_use]
extern crate lazy_static;

pub mod ast;
pub mod parser;
pub mod runtime;

fn main() {
    let input = r#"
        fn gcd(a, b) {
            if b == 0 {
                return a
            } else {
                return gcd(b, a % b)
            }
        }

        fn add(a, b) = a + b

        fn test(a) {
            fn oof(b) {
                return gcd(a, b)
            }

            return oof
        }

        return test(60)(123)
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

    match result {
        Err(err) => eprintln!("Runtime Error: {}", err),
        Ok(None) => println!("No return value"),
        Ok(Some(val)) => println!("{:?}", val),
    }
}
