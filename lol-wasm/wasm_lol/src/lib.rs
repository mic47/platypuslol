use std::collections::HashMap;

use nfa::NFA;
use redirect::{
    create_parser, resolve_parsed_output, resolve_suggestion_output, ConfigLinkQuery, LinkToken,
    QueryToken, ResolvedParsedOutput,
};
use serde::{Deserialize, Serialize};
use wasm_bindgen::prelude::*;

#[derive(Deserialize)]
pub struct JsConfig {
    substitutions: HashMap<String, Vec<HashMap<String, String>>>,
    redirects: Vec<ConfigLinkQuery<String>>,
}

#[wasm_bindgen]
pub struct ExtensionParser {
    parser: NFA<(Vec<LinkToken>, Vec<QueryToken>)>,
}

#[wasm_bindgen]
impl ExtensionParser {
    #[wasm_bindgen]
    pub fn redirect(&self, text: &str) -> Option<String> {
        let (parsed, _) = self.parser.parse_full_and_suggest(text);
        if let Some(p) = parsed.into_iter().next() {
            let ResolvedParsedOutput {
                score: _,
                link,
                description: _,
            } = resolve_parsed_output(p);
            Some(link)
        } else {
            None
        }
    }

    #[wasm_bindgen]
    pub fn suggest(&self, text: &str) -> String {
        let (parsed, suggestions) = self.parser.parse_full_and_suggest(text);
        let mut output = vec![];
        for p in parsed.into_iter() {
            let ResolvedParsedOutput {
                score: _,
                link,
                description,
            } = resolve_parsed_output(p);
            output.push(Suggestion {
                text: description,
                link,
            })
        }
        for s in suggestions.into_iter() {
            let text = resolve_suggestion_output(s);
            output.push(Suggestion {
                text: text.clone(),
                link: text,
            })
        }

        serde_json::to_string(&output).unwrap()
    }
}

#[wasm_bindgen]
pub fn init_parser(js_config: &str) -> ExtensionParser {
    let config: JsConfig = serde_json::from_str(js_config).unwrap();
    ExtensionParser {
        parser: create_parser(config.redirects, config.substitutions),
    }
}

#[wasm_bindgen]
#[derive(Serialize)]
pub struct Suggestion {
    text: String,
    link: String,
}
