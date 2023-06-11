use lol_wasm::{DslWord, NFA};

use std::path::PathBuf;

use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ConfigLinkQuery {
    query: String,
    link: String,
}

pub struct Query {
    pub parser: NFA<String>,
    pub source: ConfigLinkQuery,
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
    let config: Vec<ConfigLinkQuery> =
        serde_json::from_str(&std::fs::read_to_string(cli.link_config).unwrap()).unwrap();
    let parser = NFA::any_of(
        &(config
            .into_iter()
            // TODO: replace screw up suggestions
            .map(|c| {
                let sentence = DslWord::parse_sentence(&c.query);
                let mut prev = None;
                let mut parsers = vec![];
                for word in sentence {
                    if let Some(prev) = prev {
                        match word {
                            DslWord::QueryString(_) => {
                                parsers.push(NFA::match_one_or_more_spaces())
                            }
                            DslWord::Query(_) => parsers.push(NFA::match_one_or_more_spaces()),
                            DslWord::SubstitutionQuery(_) => (),
                            _ => match prev {
                                DslWord::QueryString(_) => {
                                    parsers.push(NFA::match_one_or_more_spaces())
                                }
                                DslWord::Query(_) => parsers.push(NFA::match_one_or_more_spaces()),
                                DslWord::SubstitutionQuery(_) => {
                                    parsers.push(NFA::match_one_or_more_spaces())
                                }
                                _ => parsers.push(NFA::match_zero_or_more_spaces()),
                            },
                        }
                    }
                    parsers.push(match word {
                        DslWord::Prefix(ref word) => NFA::match_non_empty_prefixes(&word),
                        DslWord::Optional(ref word) => {
                            NFA::any_of(&[NFA::match_string(&word), NFA::match_string("")])
                        }
                        // eat everything
                        DslWord::QueryString(ref word) => NFA::rest_of_string("<QUERY>".into()),
                        // Single word
                        DslWord::Query(ref word) => NFA::word("<WORD>".into()),
                        DslWord::SubstitutionQuery(_) => todo!(),
                    });
                    prev = Some(word)
                }
                NFA::chain(&parsers).map(&move |x| (x.clone(), c.clone()))
            })
            .collect::<Vec<_>>()),
    );
    let output = parser.parse_full_and_suggest(&cli.query);
    println!("{:#?}", output);
}