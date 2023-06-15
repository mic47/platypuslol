use std::collections::HashMap;
use std::sync::RwLock;

use nfa::NFA;
use redirect::{create_parser, resolve_parsed_output, ConfigLinkQuery, LinkToken};
use serde::{Deserialize, Serialize};
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
#[derive(Deserialize)]
pub struct JsQueryLink {
    query: String,
    link: String,
}

impl From<JsQueryLink> for ConfigLinkQuery<String> {
    fn from(c: JsQueryLink) -> Self {
        Self {
            query: c.query.clone(),
            link: c.link.clone(),
        }
    }
}

#[wasm_bindgen]
#[derive(Deserialize)]
pub struct JsConfig {
    substitutions: HashMap<String, Vec<HashMap<String, String>>>,
    redirects: Vec<JsQueryLink>,
}

lazy_static::lazy_static! {
  static ref PARSERS: RwLock<HashMap<String, NFA<Vec<LinkToken>>>> = RwLock::new(Default::default());
}

#[wasm_bindgen]
pub fn init_parser(parser_name: &str, js_config: &str) {
    let config: JsConfig = serde_json::from_str(js_config).unwrap();
    let redirects: Vec<ConfigLinkQuery<String>> =
        config.redirects.into_iter().map(Into::into).collect();
    let parser = create_parser(redirects, config.substitutions);
    PARSERS.write().unwrap().insert(parser_name.into(), parser);
}

#[wasm_bindgen]
pub fn redirect(parser_name: &str, text: &str) -> Option<String> {
    let lock = PARSERS.read().unwrap();
    let parser = lock.get(parser_name).unwrap();
    let (parsed, _) = parser.parse_full_and_suggest(text);
    for p in parsed.into_iter() {
        let (_score, link) = resolve_parsed_output(p);
        return Some(link);
    }
    None
}

#[wasm_bindgen]
#[derive(Serialize)]
pub struct Suggestion {
    text: String,
    link: String,
}

#[wasm_bindgen]
pub fn suggest(parser_name: &str, text: &str) -> String {
    let lock = PARSERS.read().unwrap();
    let parser = lock.get(parser_name).unwrap();
    let (parsed, suggestions) = parser.parse_full_and_suggest(text);
    let mut output = vec![];
    for p in parsed.into_iter() {
        let (_score, link) = resolve_parsed_output(p);
        output.push(Suggestion {
            text: text.into(),
            link,
        })
    }
    for s in suggestions.into_iter() {
        output.push(Suggestion {
            text: s.suggestion.clone(),
            link: s.suggestion,
        })
    }

    serde_json::to_string(&output).unwrap()
}
