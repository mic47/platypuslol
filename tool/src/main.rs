use std::{collections::HashSet, path::PathBuf};

use redirect::{
    create_parser, resolve_parsed_output, resolve_suggestion_output, RedirectConfig,
    ResolvedParsedOutput,
};

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
    let config: RedirectConfig<String> =
        serde_json::from_str(&std::fs::read_to_string(cli.link_config).unwrap()).unwrap();
    let parser = create_parser(config.redirects, config.substitutions).unwrap();
    let (parsed, suggested) = parser.parse_full_and_suggest(&cli.query);
    for p in parsed.into_iter() {
        let ResolvedParsedOutput {
            score,
            link,
            description,
        } = resolve_parsed_output(p, &None);
        //link = link.replace(&data.identifier, &data.payload.replace(" ", "+"));
        //println!("{:#?}", p);
        println!("{:10.5}: '{}', {:#?}", score, description, link);
    }
    let mut visited: HashSet<_> = HashSet::default();
    for s in suggested.into_iter() {
        let s = resolve_suggestion_output(s, &None);
        if visited.insert(s.clone()) {
            println!("{}: {}", s.description, s.link.unwrap_or(String::default()));
        }
    }
}
