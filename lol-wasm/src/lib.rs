mod nfa_parser;
mod query_dsl;
pub use nfa_parser::*;
pub use query_dsl::*;

#[cfg(test)]
mod nfa_parser_tests;

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
    let f = format!(
        "{:#?}",
        nfa_parser::NFA::word("fooo".into()).parse_full_and_suggest("foo bar")
    );
    let t = Test {
        a: Box::new(move |x| format!("Hello {} {}", x, f)),
    };
    alert(&(t.a)(name));
}

