use lol_wasm::{AmbiguousParser, DslWord};

use std::{path::PathBuf, sync::Arc};

use clap::Parser;

use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ConfigLinkQuery {
    query: String,
    link: String,
}

pub struct Query {
    pub parser: AmbiguousParser<String>,
    pub source: ConfigLinkQuery,
}

#[derive(Parser)]
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
    let cli = Cli::parse();
    let config: Vec<ConfigLinkQuery> =
        serde_json::from_str(&std::fs::read_to_string(cli.link_config).unwrap()).unwrap();
    let parser = AmbiguousParser::any_of(
        config
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
                                parsers.push(AmbiguousParser::at_least_one_space())
                            }
                            DslWord::Query(_) => {
                                parsers.push(AmbiguousParser::at_least_one_space())
                            }
                            DslWord::SubstitutionQuery(_) => (),
                            _ => match prev {
                                DslWord::QueryString(_) => {
                                    parsers.push(AmbiguousParser::at_least_one_space())
                                }
                                DslWord::Query(_) => {
                                    parsers.push(AmbiguousParser::at_least_one_space())
                                }
                                DslWord::SubstitutionQuery(_) => {
                                    parsers.push(AmbiguousParser::at_least_one_space())
                                }
                                _ => parsers.push(AmbiguousParser::at_least_zero_space()),
                            },
                        }
                    }
                    parsers.push(match word {
                        DslWord::Prefix(ref word) => AmbiguousParser::non_empty_prefixes(&word),
                        DslWord::Optional(ref word) => AmbiguousParser::any_of(vec![
                            AmbiguousParser::string(&word),
                            AmbiguousParser::empty("".into()),
                        ]),
                        // eat everything
                        DslWord::QueryString(ref word) => AmbiguousParser::rest_of_string("<QUERY>".into()),
                        // Single word
                        DslWord::Query(ref word) => AmbiguousParser::word("<WORD>".into()),
                        DslWord::SubstitutionQuery(_) => todo!(),
                    });
                    prev = Some(word)
                }
                AmbiguousParser::chain(parsers).map(Arc::new(move |x| (x, c.clone())))
            })
            .collect(),
    );
    let output = parser.parse_then_suggest(&cli.query);
    println!("{:#?}", output);
}
