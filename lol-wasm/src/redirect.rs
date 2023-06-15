use std::collections::HashMap;

use serde::{Deserialize, Serialize};

use crate::{parse_link, parse_query, EdgeData, LinkToken, Parsed, QueryToken, Trace, NFA};

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ConfigLinkQuery<L> {
    query: String,
    link: L,
}

pub fn create_parser(
    redirects: Vec<ConfigLinkQuery<String>>,
    substitutions: HashMap<String, Vec<HashMap<String, String>>>,
) -> NFA<Vec<LinkToken>> {
    NFA::any_of(
        &(redirects
            .into_iter()
            // TODO: replace screw up suggestions
            .map(|c| {
                // TODO: handle errors here
                let sentence = parse_query(&c.query).unwrap();
                let link = parse_link(&c.link).unwrap();
                // TODO: check that substitutions exists
                let c = ConfigLinkQuery {
                    query: c.query,
                    link,
                };
                let mut prev = None;
                let mut parsers = vec![];
                for word in sentence {
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
                    prev = Some(word)
                }
                NFA::chain(&parsers).with_payload_for_final_nodes(&c.link)
            })
            .collect::<Vec<_>>()),
    )
}

pub fn resolve_parsed_output(p: Parsed<Vec<LinkToken>>) -> (f64, String) {
    let mut matches: HashMap<String, String> = HashMap::default();
    let mut substitutions: HashMap<String, HashMap<String, String>> = HashMap::default();
    for trace in p.trace.into_iter() {
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
    (
        p.score,
        p.payload
            .iter()
            .map(|x| match x {
                LinkToken::Exact(data) => data.clone(),
                LinkToken::Replacement(replacement) => {
                    if let Some(replacement) = matches.get(replacement) {
                        // TODO: we want to html escape this in better way. Probably in javascript
                        // even?
                        replacement.clone().replace(' ', "+")
                    } else {
                        "ERROR".into()
                    }
                }
                LinkToken::Substitution(type_, subtype) => {
                    if let Some(replacement) = substitutions.get(type_).and_then(|x| x.get(subtype))
                    {
                        replacement.clone()
                    } else {
                        "ERROR".into()
                    }
                }
            })
            .collect::<Vec<_>>()
            .join(""),
    )
}
