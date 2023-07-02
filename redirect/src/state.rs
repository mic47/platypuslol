use std::collections::HashMap;

use nfa::NFA;

use crate::{
    create_parser, resolve_parsed_output, Config, ConfigLinkQuery, ConfigUrl, FallbackBehavior,
    LinkToken, QueryToken, RedirectConfig,
};

pub struct Fallback {
    pub parser: NFA<(Vec<LinkToken>, Vec<QueryToken>)>,
    pub behavior: FallbackBehavior,
}

pub struct CommonAppState {
    pub fallback: Fallback,
    pub parser: NFA<(Vec<LinkToken>, Vec<QueryToken>)>,
    pub local_configs: HashMap<String, String>,
}

impl CommonAppState {
    pub fn new(
        loaded_config: Config<String, RedirectConfig<String>>,
        local_configs: HashMap<String, String>,
    ) -> Result<Self, String> {
        let Config {
            fallback,
            redirects,
            external_configurations,
        } = loaded_config;
        let mut parsers = vec![create_parser(redirects.redirects, redirects.substitutions)
            .map_err(|err| format!("Unable to create parser from main config: {}", err))?];
        for (url, config) in external_configurations.into_iter() {
            if !config.enabled {
                continue;
            }
            if let Some(redirects) = config.config {
                let maybe_parser = create_parser_with_optional_prefix(
                    redirects.redirects,
                    redirects.substitutions,
                    config.prefix,
                )
                .map_err(|err| format!("Unable to create parser from config {:?}: {}", url, err));
                match url {
                    ConfigUrl::Builtin { path: _ } | ConfigUrl::Local { path: _ } => {
                        parsers.push(maybe_parser?);
                    }
                    ConfigUrl::Remote { url: _ } => match maybe_parser {
                        Ok(parser) => parsers.push(parser),
                        Err(err) => eprintln!("{}", err),
                    },
                }
            }
        }
        Ok(CommonAppState {
            parser: NFA::any_of(&parsers),
            local_configs,
            fallback: Fallback {
                behavior: fallback.clone(),
                parser: create_parser(
                    vec![ConfigLinkQuery {
                        query: fallback.make_query("{query:query}"),
                        link: fallback.link,
                    }],
                    Default::default(),
                )
                .map_err(|err| {
                    format!(
                        "Unable to create parser {}. Substitution should be named 'query'",
                        err
                    )
                })?,
            },
        })
    }

    pub fn redirect(&self, query: &str) -> Option<String> {
        let parsers = vec![
            Some((query.into(), &self.parser)),
            if self.fallback.behavior.redirect_automatically {
                Some((
                    self.fallback.behavior.make_query(query),
                    &self.fallback.parser,
                ))
            } else {
                None
            },
        ];
        for (query, parser) in parsers.into_iter().flatten() {
            let (parsed, _) = parser.parse_full_and_suggest(&query);
            if let Some(p) = parsed
                .into_iter()
                .map(|x| resolve_parsed_output(x, &None))
                .next()
            {
                return Some(p.link);
            }
        }
        None
    }
}

#[allow(clippy::type_complexity)]
fn create_parser_with_optional_prefix(
    redirects: Vec<ConfigLinkQuery<String>>,
    substitutions: HashMap<String, Vec<HashMap<String, String>>>,
    prefix: Option<String>,
) -> Result<NFA<(Vec<LinkToken>, Vec<QueryToken>)>, String> {
    if let Some(prefix) = prefix {
        create_parser(
            redirects.into_iter().map(|x| x.prefix(&prefix)).collect(),
            substitutions,
        )
        .map_err(|err| format!("Unable to create parser {}", err))
    } else {
        create_parser(redirects, substitutions)
            .map_err(|err| format!("Unable to create parser {}", err))
    }
}
