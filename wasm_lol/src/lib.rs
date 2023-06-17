use std::collections::{HashMap, HashSet};

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
            } = resolve_parsed_output(p, &None);
            Some(link)
        } else {
            None
        }
    }

    #[wasm_bindgen]
    pub fn suggest(&self, text: &str) -> Result<String, String> {
        let (parsed, suggestions) = self.parser.parse_full_and_suggest(text);
        let mut output = vec![];
        for p in parsed.into_iter() {
            let ResolvedParsedOutput {
                score: _,
                link,
                description,
            } = resolve_parsed_output(p, &None);
            output.push(Suggestion {
                text: description,
                link,
            })
        }
        let mut visited: HashSet<_> = HashSet::default();
        for s in suggestions.into_iter() {
            let s = resolve_suggestion_output(s, &None);
            if visited.insert(s.clone()) {
                output.push(Suggestion {
                    text: s.description,
                    link: s.link.unwrap_or(Default::default()),
                })
            }
        }
        serde_json::to_string(&output).map_err(|x| x.to_string())
    }
}

#[wasm_bindgen]
pub fn init_parser(js_config: &str) -> Result<ExtensionParser, String> {
    let config: JsConfig = serde_path_to_error::deserialize(
        serde_json::from_str::<serde_json::Value>(js_config).map_err(|x| x.to_string())?,
    )
    .map_err(|x| x.to_string())?;
    Ok(ExtensionParser {
        parser: create_parser(config.redirects, config.substitutions)?,
    })
}

#[wasm_bindgen]
#[derive(Serialize)]
pub struct Suggestion {
    text: String,
    link: String,
}
