use std::collections::HashMap;

use anyhow::Context;

use nfa::NFA;

use crate::{
    create_parser, resolve_parsed_output, Behavior, Config, ConfigLinkQuery, ConfigUrl,
    FallbackBehavior, LinkToken, QueryToken, RedirectConfig,
};

pub struct Fallback {
    pub parser: NFA<(Vec<Vec<LinkToken>>, Vec<QueryToken>)>,
    pub behavior: FallbackBehavior,
}

pub struct CommonAppState {
    pub fallback: Fallback,
    pub behavior: Behavior,
    pub parser: NFA<(Vec<Vec<LinkToken>>, Vec<QueryToken>)>,
    pub loaded_config: Config<String, RedirectConfig<String>>,
}

impl CommonAppState {
    pub fn new(loaded_config: Config<String, RedirectConfig<String>>) -> anyhow::Result<Self> {
        let Config {
            ref fallback,
            ref behavior,
            ref redirects,
            ref external_configurations,
        } = loaded_config;
        let mut substitutions = redirects.substitutions.clone();
        for (t, ext_conf) in external_configurations.iter() {
            if let Some(ref conf) = ext_conf.config {
                for subst_key in ext_conf.substitutions_to_inherit.iter() {
                    if let Some(subst_value) = conf.substitutions.get(subst_key) {
                        if substitutions.contains_key(subst_key) {
                            return Err(anyhow::anyhow!(
                                "Substitution key '{}' from '{:?}' already exists",
                                subst_key,
                                t
                            ));
                        } else {
                            substitutions.insert(subst_key.clone(), subst_value.clone());
                        }
                    }
                }
            }
        }
        let mut parsers = vec![create_parser(redirects.redirects.clone(), substitutions)
            .context("Unable to create parser from main config")?];
        for (url, config) in external_configurations.iter() {
            if !config.enabled {
                continue;
            }
            if let Some(ref redirects) = config.config {
                let maybe_parser = create_parser_with_optional_prefix(
                    redirects.redirects.clone(),
                    redirects.substitutions.clone(),
                    config.prefix.clone(),
                )
                .with_context(|| format!("Unable to create parser from config {:?}", url));
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
            behavior: behavior.clone(),
            fallback: Fallback {
                behavior: fallback.clone(),
                parser: create_parser(
                    vec![ConfigLinkQuery {
                        query: fallback.make_query("{query:query}"),
                        exact: false,
                        links: vec![fallback.link.clone()],
                    }],
                    Default::default(),
                )
                .context("Unable to create parser. Substitution should be name 'query'.")?,
            },
            loaded_config,
        })
    }

    pub fn redirect(&self, query: &str) -> Option<Vec<String>> {
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
            if !self.behavior.pick_first_in_conflict && parsed.len() > 1 {
                return None;
            }
            if let Some((p, _)) = parsed
                .into_iter()
                .map(|x| resolve_parsed_output(x, &None))
                .next()
            {
                return Some(p.links);
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
) -> anyhow::Result<NFA<(Vec<Vec<LinkToken>>, Vec<QueryToken>)>> {
    if let Some(prefix) = prefix {
        create_parser(
            redirects.into_iter().map(|x| x.prefix(&prefix)).collect(),
            substitutions,
        )
        .context("Unable to create parser")
    } else {
        create_parser(redirects, substitutions).context("Unable to create parser")
    }
}
