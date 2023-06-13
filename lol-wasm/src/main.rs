use lol_wasm::{parse_link, parse_query, EdgeData, LinkToken, QueryToken, NFA};

use std::{collections::HashMap, path::PathBuf};

use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Config {
    substitutions: HashMap<String, Vec<HashMap<String, String>>>,
    redirects: Vec<ConfigLinkQuery<String>>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ConfigLinkQuery<L> {
    query: String,
    link: L,
}

#[derive(clap::Parser)]
struct Cli {
    #[arg(short, long, value_name = "FILE")]
    pub link_config: PathBuf,

    #[arg(short, long)]
    pub query: String,
}

pub fn main() {
    // TODO:
    // - [ ] Read config from file
    // - [ ] Construct basic parser
    // - [ ] Construct more advanced parser (actually use the DSL)
    // - [ ] Add substitutions
    let cli = <Cli as clap::Parser>::parse();
    let config: Config =
        serde_json::from_str(&std::fs::read_to_string(cli.link_config).unwrap()).unwrap();
    let parser = NFA::any_of(
        &(config
            .redirects
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
                        QueryToken::Exact(ref word) => NFA::match_string(&word),
                        QueryToken::Prefix(ref word) => NFA::match_non_empty_prefixes(&word),
                        QueryToken::Regex(ref identifier, ref regex) => {
                            NFA::regex(identifier.clone(), regex.clone())
                        }
                        QueryToken::Substitution(ref identifier, ref type_, ref subtype) => {
                            NFA::substitution(
                                identifier.clone(),
                                config
                                    .substitutions
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
    );
    let (parsed, suggested) = parser.parse_full_and_suggest(&cli.query);
    for p in parsed.into_iter() {
        let mut matches: HashMap<String, String> = HashMap::default();
        let mut substitutions: HashMap<String, HashMap<String, String>> = HashMap::default();
        for trace in p.trace.into_iter() {
            match trace {
                lol_wasm::Trace::Edge(data) => {
                    match data.payload {
                        EdgeData::Match(replacement) => {
                            matches.insert(data.identifier.clone(), replacement);
                        }
                        EdgeData::Substitution(substitution) => {
                            substitutions.insert(data.identifier.clone(), substitution);
                        }
                    };
                }
                lol_wasm::Trace::Node(_) => (),
            }
        }
        let link = p.payload
            .iter()
            .map(|x| match x {
                LinkToken::Exact(data) => data.clone(),
                LinkToken::Replacement(replacement) => {
                    if let Some(replacement) = matches.get(replacement) {
                        // TODO: we want to html escape this in better way. Probably in javascript
                        // even?
                        replacement.clone().replace(" ", "+")
                    } else {
                        "ERROR".into()
                    }
                }
                LinkToken::Substitution(type_, subtype) => {
                    if let Some(replacement) =
                        substitutions.get(type_).and_then(|x| x.get(subtype))
                    {
                        replacement.clone()
                    } else {
                        "ERROR".into()
                    }
                }
            })
            .collect::<Vec<_>>()
            .join("");
        //link = link.replace(&data.identifier, &data.payload.replace(" ", "+"));
        //println!("{:#?}", p);
        println!("{:10.5}: {:#?}", p.score, link);
    }
    for s in suggested.into_iter() {
        println!("{:#?}", s);
    }
}
