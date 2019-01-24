#[macro_use]
extern crate lazy_static;

mod ast;
mod parser;
mod runtime;

use std::ops::Deref;
use runtime::object::*;
use runtime::gc::*;

fn main() {
    // let input = r#"
    //     c =
    // "#;
    // let mut parser = parser::Parser::new(input);
    // match parser.program() {
    //     Ok(ast) => println!("{:#?}", ast),
    //     Err(err) => eprintln!("Error: {}", err)
    // }

    let debug_heap = |heap: &Heap| println!("Objects: {}", heap.heap.len());

    let mut heap = Heap::new();
    let roots: Vec<GcHandle> = Vec::new();

    let item = {
        let b = heap.construct(Object::Int(10));
        debug_heap(&heap);

        let mut vec = heap.construct(Object::Array(Array {
            items: vec![b.as_gc()]
        }));

        let vec_gc = vec.as_gc();
        if let Object::Array(ref mut array) = *vec {
            array.items.push(vec_gc);
        }

        b
    };

    debug_heap(&heap);
    heap.gc(roots.iter());
    debug_heap(&heap);
}
