use std::{
    collections::{HashMap, HashSet},
    sync::Arc,
};

use anyhow::Context;
use redirect::{resolve_parsed_output, resolve_suggestion_output, CommonAppState};

pub fn debug(
    request_params: &HashMap<String, String>,
    state: Arc<CommonAppState>,
) -> anyhow::Result<Option<serde_json::Value>> {
    if let Some(query) = request_params.get("q") {
        let (parsed, suggested) = state.parser.parse_full_and_suggest(query);
        let parsed: Vec<_> = parsed
            .into_iter()
            .map(|x| resolve_parsed_output(x, &None).0)
            .collect();
        let suggested: Vec<_> = suggested
            .into_iter()
            .map(|x| resolve_suggestion_output(x, &None).0)
            .collect();
        Ok(Some(
            serde_json::to_value((parsed, suggested)).context("Unable to serialize to json")?,
        ))
    } else {
        Ok(None)
    }
}

pub fn suggest(
    query_params: &HashMap<String, String>,
    state: Arc<CommonAppState>,
) -> anyhow::Result<Option<serde_json::Value>> {
    let mut suggestions_left: i32 = 20; // TODO: configure
    if let Some(query) = query_params.get("q") {
        let (parsed, suggested) = state.parser.parse_full_and_suggest(query);
        let mut suggested_queries: Vec<String> = vec![];
        let mut suggested_texts: Vec<String> = vec![];
        let mut suggested_urls: Vec<String> = vec![];
        for p in parsed
            .into_iter()
            .map(|x| resolve_parsed_output(x, &None).0)
        {
            suggested_texts.push(format!("{} => {:?}", p.description, p.links));
            suggested_queries.push(p.description.clone());
            if let [link] = &p.links[..] {
                suggested_urls.push(link.clone());
            } else {
                suggested_urls.push(format!("redirect?q={}", p.description));
            }
            suggestions_left -= 1;
            if suggestions_left <= 0 {
                break;
            }
        }
        let mut visited: HashSet<_> = HashSet::default();
        for s in suggested
            .into_iter()
            .map(|x| resolve_suggestion_output(x, &None).0)
        {
            if !visited.insert(s.clone()) {
                continue;
            }
            suggested_texts.push(format!(
                "{} ➡️  {:?}",
                s.description,
                s.links.as_ref().unwrap_or(&vec!["???".into()])
            ));
            suggested_queries.push(s.description.clone());
            if let [link] = &s.links.unwrap_or(vec!["???".into()])[..] {
                suggested_urls.push(link.clone());
            } else {
                suggested_urls.push(format!("redirect?q={}", s.description));
            }
            suggestions_left -= 1;
            if suggestions_left <= 0 {
                break;
            }
        }

        Ok(Some(
            serde_json::to_value((query, suggested_queries, suggested_texts, suggested_urls))
                .context("Unable to serialize suggestions")?,
        ))
    } else {
        Ok(None)
    }
}
