use std::collections::HashSet;

use redirect::{
    resolve_parsed_output, resolve_suggestion_output, CommonAppState, Config, RedirectConfig,
    ResolvedParsedOutput,
};
use serde::Serialize;
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub struct ExtensionParser {
    state: CommonAppState,
}

#[wasm_bindgen]
impl ExtensionParser {
    #[wasm_bindgen]
    pub fn redirect(&self, text: &str) -> Option<String> {
        self.state.redirect(text)
    }

    #[wasm_bindgen]
    pub fn suggest(&self, text: &str) -> Result<String, String> {
        let (parsed, suggestions) = self.state.parser.parse_full_and_suggest(text);
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
    let config: Config<String, RedirectConfig<String>> = serde_path_to_error::deserialize(
        &serde_json::from_str::<serde_json::Value>(js_config).map_err(|x| x.to_string())?,
    )
    .map_err(|x| x.to_string())?;
    let app_state = CommonAppState::new(config)?;
    Ok(ExtensionParser { state: app_state })
}

#[wasm_bindgen]
#[derive(Serialize)]
pub struct Suggestion {
    text: String,
    link: String,
}
