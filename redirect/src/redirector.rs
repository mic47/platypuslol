use std::collections::HashMap;

use anyhow::Context;
use serde::Serialize;

use crate::{
    parse_link, parse_query, validate_query_and_link_with_substitutions, validate_query_with_link,
    LinkToken, QueryToken,
};
use nfa::{EdgeData, Parsed, Suggestion, Trace, NFA};

#[derive(Clone, Debug)]
pub struct ConfigLinkQuery<L> {
    pub query: String,
    pub links: Vec<L>,
    pub exact: bool,
}

impl<L: Clone> ConfigLinkQuery<L> {
    pub fn prefix(&self, prefix: &str) -> Self {
        Self {
            query: format!("{} {}", prefix, &self.query),
            links: self.links.clone(),
            exact: self.exact,
        }
    }
}

#[allow(clippy::type_complexity)]
pub fn create_parser(
    redirects: Vec<ConfigLinkQuery<String>>,
    substitutions: HashMap<String, Vec<HashMap<String, String>>>,
) -> anyhow::Result<NFA<(Vec<Vec<LinkToken>>, Vec<QueryToken>)>> {
    Ok(NFA::any_of(
        &(redirects
            .into_iter()
            // TODO: replace screw up suggestions
            .map(|c| {
                // TODO: handle errors here
                let sentence = parse_query(&c.query, c.exact)
                    .with_context(|| format!("Unable to parse query '{}'", c.query))?;
                let links = c
                    .links
                    .iter()
                    .map(|link| {
                        let parsed_link: Vec<LinkToken> = parse_link(link)
                            .with_context(|| format!("Unable to parse link '{}'", link))?;
                        validate_query_with_link(&sentence, &c.query, &parsed_link, link)
                            .with_context(|| {
                                format!("Unable to validate '{}' with '{}'", c.query, link)
                            })?;
                        validate_query_and_link_with_substitutions(
                            &sentence,
                            &parsed_link,
                            &substitutions,
                        )
                        .with_context(|| {
                            format!(
                                "Unable to validate substitutions in '{}' with '{}'",
                                c.query, link
                            )
                        })?;
                        anyhow::Result::<_>::Ok(parsed_link)
                    })
                    .collect::<Result<Vec<_>, _>>()?;
                let c = ConfigLinkQuery {
                    query: c.query,
                    exact: c.exact,
                    links,
                };
                let mut prev = None;
                let mut parsers = vec![];
                for word in sentence.iter() {
                    if let Some(prev) = prev {
                        match word {
                            QueryToken::Regex(_, _) => {
                                parsers.push(NFA::match_one_or_more_spaces());
                            }
                            QueryToken::Substitution { .. } => match prev {
                                QueryToken::Regex(_, _) | QueryToken::Substitution { .. } => {
                                    parsers.push(NFA::match_one_or_more_spaces())
                                }
                                QueryToken::Exact(_) | QueryToken::Prefix { .. } => {
                                    parsers.push(NFA::match_one_or_more_spaces())
                                }
                            },
                            QueryToken::Exact(_) | QueryToken::Prefix { .. } => match prev {
                                QueryToken::Regex(_, _) | QueryToken::Substitution { .. } => {
                                    parsers.push(NFA::match_one_or_more_spaces());
                                }
                                QueryToken::Exact(_) | QueryToken::Prefix { .. } => {
                                    parsers.push(NFA::match_zero_or_more_spaces())
                                }
                            },
                        }
                    }
                    parsers.push(match word {
                        QueryToken::Exact(ref word) => NFA::match_string(word),
                        QueryToken::Prefix { ref word } => NFA::match_non_empty_prefixes(word),
                        QueryToken::Regex(ref identifier, ref regex) => {
                            NFA::regex(identifier.clone(), regex.clone())
                        }
                        QueryToken::Substitution {
                            ref name,
                            ref type_,
                            ref subtype,
                        } => NFA::substitution(
                            name.clone(),
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
                        ),
                    });
                    prev = Some(word.clone())
                }
                Ok(NFA::chain(&parsers).with_payload_for_final_nodes(&(c.links, sentence)))
            })
            .collect::<anyhow::Result<Vec<_>>>())?,
    ))
}

#[derive(Serialize, Debug)]
pub struct ResolvedParsedOutput {
    pub score: f64,
    pub links: Vec<String>,
    pub description: String,
}

#[derive(Clone, Debug)]
pub struct ResolvedOutputMetadata {
    pub command: Vec<QueryToken>,
    pub query: HashMap<String, String>,
    pub substitutions: HashMap<String, HashMap<String, String>>,
}

type Sentence = Vec<LinkToken>;

fn process_trace(
    trace: Vec<Trace<&(Vec<Sentence>, Vec<QueryToken>)>>,
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

pub fn resolve_parsed_output(
    p: Parsed<(Vec<Vec<LinkToken>>, Vec<QueryToken>)>,
    default_replacement: &Option<String>,
) -> (ResolvedParsedOutput, ResolvedOutputMetadata) {
    // TODO: check if you need clone to vec
    let (matches, substitutions) = process_trace(p.trace.clone_to_vec());
    (
        ResolvedParsedOutput {
            score: p.score,
            links: p
                .payload
                .0
                .iter()
                .filter_map(|payload| {
                    process_query(&matches, &substitutions, payload, default_replacement)
                })
                .collect(),
            description: process_suggestion(
                &matches,
                &substitutions,
                &p.payload.1,
                default_replacement,
            )
            .join(" "),
        },
        ResolvedOutputMetadata {
            command: p.payload.1.clone(),
            query: matches,
            substitutions,
        },
    )
}

fn process_query(
    matches: &HashMap<String, String>,
    substitutions: &HashMap<String, HashMap<String, String>>,
    query: &[LinkToken],
    default_replacement: &Option<String>,
) -> Option<String> {
    Some(
        query
            .iter()
            .map(|x| {
                Some(match x {
                    LinkToken::Exact(data) => data.clone(),
                    LinkToken::Replacement(replacement) => {
                        if let Some(replacement) = matches.get(replacement) {
                            // TODO: we want to html escape this in better way. Probably in javascript
                            // even?
                            replacement.clone().replace(' ', "+")
                        } else {
                            // If we cannot produce the link, let's not return the link
                            default_replacement.clone()?
                        }
                    }
                    LinkToken::Substitution { name, subtype } => {
                        if let Some(replacement) =
                            substitutions.get(name).and_then(|x| x.get(subtype))
                        {
                            replacement.clone()
                        } else {
                            // If we cannot produce the link, let's not return the link
                            None?
                        }
                    }
                })
            })
            .collect::<Option<Vec<_>>>()?
            .join(""),
    )
}

pub fn process_suggestion(
    matches: &HashMap<String, String>,
    substitutions: &HashMap<String, HashMap<String, String>>,
    query: &[QueryToken],
    default_replacement: &Option<String>,
) -> Vec<String> {
    query
        .iter()
        .map(|x| x.to_description(matches, substitutions, default_replacement, false))
        .collect::<Vec<_>>()
}

#[derive(Debug, Clone, Hash, PartialEq, Eq, Serialize)]
pub struct ResolvedSuggestionOutput {
    pub links: Option<Vec<String>>,
    pub description: String,
}

pub fn resolve_suggestion_output(
    suggestion: Suggestion<(Vec<Vec<LinkToken>>, Vec<QueryToken>)>,
    default_replacement: &Option<String>,
) -> (ResolvedSuggestionOutput, ResolvedOutputMetadata) {
    let (matches, substitutions) = process_trace(suggestion.trace);
    (
        ResolvedSuggestionOutput {
            description: suggestion
                .payload
                .as_ref()
                .map(|x| {
                    process_suggestion(&matches, &substitutions, &x.1, default_replacement)
                        .join(" ")
                })
                .unwrap_or(suggestion.suggestion.clone()),
            links: suggestion.payload.as_ref().map(|x| {
                x.0.iter()
                    .filter_map(|link| {
                        process_query(&matches, &substitutions, link, default_replacement)
                    })
                    .collect()
            }),
        },
        ResolvedOutputMetadata {
            command: suggestion
                .payload
                .clone()
                .map(|x| x.1)
                .unwrap_or_else(|| vec![QueryToken::Exact(suggestion.suggestion.clone())]),
            query: matches,
            substitutions,
        },
    )
}
