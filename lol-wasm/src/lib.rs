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
        // TODO regex module is in wasm. Consider something smaller
        nfa_parser::NFA::regex("fooo".into(), regex::Regex::new(r"\w+").unwrap())
            .parse_full_and_suggest("foo bar")
    );
    let t = Test {
        a: Box::new(move |x| format!("Hello {} {}", x, f)),
    };
    alert(&(t.a)(name));
}

