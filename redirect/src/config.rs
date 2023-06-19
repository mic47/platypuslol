use std::collections::HashMap;

use serde::{Deserialize, Serialize};

use crate::ConfigLinkQuery;

#[derive(Clone, Debug, Serialize, Deserialize)]
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

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ExternalParser<R> {
    pub enabled: bool,
    #[serde(default)]
    pub prefix: Option<String>,
    #[serde(skip, default = "default_none")]
    pub config: Option<R>,
}

fn default_none<T>() -> Option<T> {
    None
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Config<T, R> {
    pub fallback: FallbackBehavior,
    #[serde(flatten)]
    pub redirects: RedirectConfig<T>,
    pub external_configurations: HashMap<ConfigUrl, ExternalParser<R>>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct FallbackBehavior {
    pub link: String,
    pub redirect_automatically: bool,
    pub query_prefix: String,
}

impl FallbackBehavior {
    pub fn make_query(&self, payload: &str) -> String {
        format!("{} {}", self.query_prefix, payload)
    }
}
