use std::collections::HashMap;

use serde::{Deserialize, Serialize};

use crate::{parse_link, parse_query, validate_query_with_link, LinkToken, QueryToken};
use nfa::{EdgeData, Parsed, Suggestion, Trace, NFA};

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ConfigLinkQuery<L> {
    pub query: String,
    pub link: L,
}

pub fn create_parser(
    redirects: Vec<ConfigLinkQuery<String>>,
    substitutions: HashMap<String, Vec<HashMap<String, String>>>,
) -> Result<NFA<(Vec<LinkToken>, Vec<QueryToken>)>, String> {
    Ok(NFA::any_of(
        &(redirects
            .into_iter()
            // TODO: replace screw up suggestions
            .map(|c| {
                // TODO: handle errors here
                let sentence = parse_query(&c.query)?;
                let link = parse_link(&c.link)?;
                validate_query_with_link(&sentence, &c.query, &link, &c.link)?;
                let c = ConfigLinkQuery {
                    query: c.query,
                    link,
                };
                let mut prev = None;
                let mut parsers = vec![];
                for word in sentence.iter() {
                    if let Some(prev) = prev {
                        match word {
                            QueryToken::Regex(_, _) => {
                                parsers.push(NFA::match_one_or_more_spaces());
                            }
                            QueryToken::Substitution(_, _, _) => {
                                parsers.push(NFA::match_one_or_more_spaces());
                            }
                            _ => match prev {
                                QueryToken::Regex(_, _) => {
                                    parsers.push(NFA::match_one_or_more_spaces())
                                }
                                QueryToken::Substitution(_, _, _) => {
                                    parsers.push(NFA::match_one_or_more_spaces());
                                }
                                _ => parsers.push(NFA::match_zero_or_more_spaces()),
                            },
                        }
                    }
                    parsers.push(match word {
                        QueryToken::Exact(ref word) => NFA::match_string(word),
                        QueryToken::Prefix(ref word) => NFA::match_non_empty_prefixes(word),
                        QueryToken::Regex(ref identifier, ref regex) => {
                            NFA::regex(identifier.clone(), regex.clone())
                        }
                        QueryToken::Substitution(ref identifier, ref type_, ref subtype) => {
                            NFA::substitution(
                                identifier.clone(),
                                substitutions
                                    .get(type_)
                                    .map(|x| {
                                        x.iter()
                                            .filter_map(|x| {
                                                x.get(subtype).map(|y| (y.clone(), x.clone()))
                                            })
                                            .collect()
                                    })
                                    .unwrap_or_default(),
                            )
                        }
                    });
                    prev = Some(word.clone())
                }
                Ok(NFA::chain(&parsers).with_payload_for_final_nodes(&(c.link, sentence)))
            })
            .collect::<Result<Vec<_>, String>>())?,
    ))
}

pub struct ResolvedParsedOutput {
    pub score: f64,
    pub link: String,
    pub description: String,
}

fn process_trace(
    trace: Vec<Trace<&(Vec<LinkToken>, Vec<QueryToken>)>>,
) -> (
    HashMap<String, String>,
    HashMap<String, HashMap<String, String>>,
) {
    let mut matches: HashMap<String, String> = HashMap::default();
    let mut substitutions: HashMap<String, HashMap<String, String>> = HashMap::default();
    for trace in trace.into_iter() {
        match trace {
            Trace::Edge(data) => {
                match data.payload {
                    EdgeData::Match(replacement) => {
                        matches.insert(data.identifier.clone(), replacement);
                    }
                    EdgeData::Substitution(substitution) => {
                        substitutions.insert(data.identifier.clone(), substitution);
                    }
                };
            }
            Trace::Node(_) => (),
        }
    }
    (matches, substitutions)
}

pub fn resolve_parsed_output(p: Parsed<(Vec<LinkToken>, Vec<QueryToken>)>) -> ResolvedParsedOutput {
    let (matches, substitutions) = process_trace(p.trace);
    ResolvedParsedOutput {
        score: p.score,
        link: process_query(&matches, &substitutions, &p.payload.0),
        description: process_suggestion(&matches, &substitutions, &p.payload.1),
    }
}

fn process_query(
    matches: &HashMap<String, String>,
    substitutions: &HashMap<String, HashMap<String, String>>,
    query: &[LinkToken],
) -> String {
    query
        .iter()
        .map(|x| match x {
            LinkToken::Exact(data) => data.clone(),
            LinkToken::Replacement(replacement) => {
                if let Some(replacement) = matches.get(replacement) {
                    // TODO: we want to html escape this in better way. Probably in javascript
                    // even?
                    replacement.clone().replace(' ', "+")
                } else {
                    "<query>".into()
                }
            }
            LinkToken::Substitution(type_, subtype) => {
                if let Some(replacement) = substitutions.get(type_).and_then(|x| x.get(subtype)) {
                    replacement.clone()
                } else {
                    format!("<{}>", type_)
                }
            }
        })
        .collect::<Vec<_>>()
        .join("")
}

fn process_suggestion(
    matches: &HashMap<String, String>,
    substitutions: &HashMap<String, HashMap<String, String>>,
    query: &[QueryToken],
) -> String {
    query
        .iter()
        .map(|x| match x {
            QueryToken::Exact(data) => data.clone(),
            QueryToken::Prefix(data) => data.clone(),
            QueryToken::Regex(replacement, _) => {
                if let Some(replacement) = matches.get(replacement) {
                    // TODO: we want to html escape this in better way. Probably in javascript
                    // even?
                    replacement.clone().replace(' ', "+")
                } else {
                    "<query>".into()
                }
            }
            QueryToken::Substitution(type_, _, subtype) => {
                if let Some(replacement) = substitutions.get(type_).and_then(|x| x.get(subtype)) {
                    replacement.clone()
                } else {
                    format!("<{}>", type_)
                }
            }
        })
        .collect::<Vec<_>>()
        .join(" ")
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct ResolvedSuggestionOutput {
    pub link: Option<String>,
    pub description: String,
}

pub fn resolve_suggestion_output(
    suggestion: Suggestion<(Vec<LinkToken>, Vec<QueryToken>)>,
) -> ResolvedSuggestionOutput {
    let (matches, substitutions) = process_trace(suggestion.trace);
    ResolvedSuggestionOutput {
        description: suggestion
            .payload
            .as_ref()
            .map(|x| process_suggestion(&matches, &substitutions, &x.1))
            .unwrap_or(suggestion.suggestion),
        link: suggestion
            .payload
            .as_ref()
            .map(|x| process_query(&matches, &substitutions, &x.0)),
    }
}
