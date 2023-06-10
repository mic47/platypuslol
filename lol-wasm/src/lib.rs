mod ambiguous_parser;
pub use ambiguous_parser::*;

use wasm_bindgen::prelude::*;

#[wasm_bindgen]
extern "C" {
    pub fn alert(s: &str);
}

struct Test<T> {
    pub a: Box<dyn Fn(&str) -> T>,
}

#[wasm_bindgen]
pub fn greet(name: &str) {
    let t = Test {
        a: Box::new(|x| format!("Hello {}", x)),
    };
    alert(&(t.a)(name));
}

