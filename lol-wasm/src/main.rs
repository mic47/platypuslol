use lol_wasm::AmbiguousParser;

use std::path::PathBuf;

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
            .map(|c| AmbiguousParser::string(c.query.clone()).replace_with(c))
            .collect(),
    );
    let output = parser.parse_then_suggest(&cli.query);
    println!("{:#?}", output);
}
