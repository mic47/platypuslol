use std::collections::HashMap;

use serde::{Deserialize, Serialize};

use crate::ConfigLinkQuery;

#[derive(Clone, Debug)]
pub struct RedirectConfig<T> {
    pub substitutions: HashMap<String, Vec<HashMap<String, String>>>,
    pub redirects: Vec<ConfigLinkQuery<T>>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum ConfigUrl {
    Builtin { path: String },
    Local { path: String },
    Remote { url: String },
}

impl<'de> Deserialize<'de> for ConfigUrl {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let url: &str = Deserialize::deserialize(deserializer)?;
        Ok(if let Some(path) = url.strip_prefix("builtin://") {
            ConfigUrl::Builtin { path: path.into() }
        } else if let Some(path) = url.strip_prefix("local://") {
            ConfigUrl::Local { path: path.into() }
        } else {
            ConfigUrl::Remote { url: url.into() }
        })
    }
}

impl Serialize for ConfigUrl {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        match self {
            ConfigUrl::Builtin { path } => serializer.serialize_str(&format!("builtin://{path}")),
            ConfigUrl::Local { path } => serializer.serialize_str(&format!("local://{path}")),
            ConfigUrl::Remote { url } => serializer.serialize_str(url),
        }
    }
}

#[derive(Clone, Debug)]
pub struct ExternalParser<R> {
    pub enabled: bool,
    pub substitutions_to_inherit: Vec<String>,
    pub prefix: Option<String>,
    pub config: Option<R>,
}

fn default_none<T>() -> Option<T> {
    None
}

#[derive(Clone, Debug)]
pub struct Config<T, R> {
    pub fallback: FallbackBehavior,
    pub behavior: Behavior,
    pub redirects: RedirectConfig<T>,
    pub external_configurations: HashMap<ConfigUrl, ExternalParser<R>>,
}

impl Config<String, ()> {
    pub fn from_config_file(config: ConfigFile<()>) -> Result<Self, String> {
        let ConfigFile {
            fallback,
            behavior,
            redirects,
            external_configurations,
        } = config;
        Ok(Config {
            fallback,
            behavior,
            redirects: RedirectConfig::from_config_file(redirects)?,
            external_configurations: external_configurations
                .into_iter()
                .map(|(k, v)| {
                    (
                        k,
                        ExternalParser {
                            enabled: v.enabled,
                            substitutions_to_inherit: v.substitutions_to_inherit,
                            prefix: v.prefix,
                            config: None,
                        },
                    )
                })
                .collect(),
        })
    }
}

impl Config<String, RedirectConfig<String>> {
    pub fn from_config_file(config: ConfigFile<RedirectConfigFile>) -> Result<Self, String> {
        let ConfigFile {
            fallback,
            behavior,
            redirects,
            external_configurations,
        } = config;
        Ok(Config {
            fallback,
            behavior,
            redirects: RedirectConfig::from_config_file(redirects)?,
            external_configurations: external_configurations
                .into_iter()
                .map(|(k, v)| {
                    (
                        k,
                        ExternalParser {
                            enabled: v.enabled,
                            substitutions_to_inherit: v.substitutions_to_inherit,
                            prefix: v.prefix,
                            config: v
                                .config
                                .and_then(|c| RedirectConfig::from_config_file(c).ok()),
                        },
                    )
                })
                .collect(),
        })
    }
}

impl RedirectConfig<String> {
    pub fn from_config_file(config: RedirectConfigFile) -> Result<Self, String> {
        Ok(RedirectConfig {
            substitutions: config.substitutions,
            redirects: config
                .redirects
                .into_iter()
                .map(|x| {
                    let mut links = vec![];
                    if x.link.is_some() && !x.links.is_empty() {
                        return Err(format!(
                            "Both link and links are non-empty for command '{}'",
                            &x.query
                        ));
                    }
                    links.extend(x.link);
                    links.extend(x.links.into_iter());
                    if links.is_empty() {
                        Err(format!("There are no links for command '{}'", &x.query))
                    } else {
                        Ok(ConfigLinkQuery {
                            query: x.query,
                            links,
                        })
                    }
                })
                .collect::<Result<_, _>>()?,
        })
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ConfigFile<R> {
    pub fallback: FallbackBehavior,
    #[serde(flatten)]
    pub behavior: Behavior,
    #[serde(flatten)]
    pub redirects: RedirectConfigFile,
    pub external_configurations: HashMap<ConfigUrl, ExternalParserFile<R>>, // TODO
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ExternalParserFile<R> {
    pub enabled: bool,
    #[serde(default)]
    pub substitutions_to_inherit: Vec<String>,
    #[serde(default)]
    pub prefix: Option<String>,
    #[serde(default = "default_none")]
    pub config: Option<R>,
}

pub type Substitutions = HashMap<String, Vec<HashMap<String, String>>>;

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct RedirectConfigFile {
    pub substitutions: Substitutions,
    pub redirects: Vec<ConfigLinkQueryFile>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ConfigLinkQueryFile {
    pub query: String,
    #[serde(default = "default_empty")]
    pub links: Vec<String>,
    #[serde(default = "default_none")]
    pub link: Option<String>,
}

fn default_empty<T>() -> Vec<T> {
    vec![]
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct FallbackBehavior {
    pub link: String,
    pub redirect_automatically: bool,
    pub query_prefix: String,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Behavior {
    pub pick_first_in_conflict: bool,
}

impl FallbackBehavior {
    pub fn make_query(&self, payload: &str) -> String {
        format!("{} {}", self.query_prefix, payload)
    }
}
