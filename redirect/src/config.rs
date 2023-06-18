use std::collections::HashMap;

use serde::{Deserialize, Serialize};

use crate::ConfigLinkQuery;

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct RedirectConfig<T> {
    pub substitutions: HashMap<String, Vec<HashMap<String, String>>>,
    pub redirects: Vec<ConfigLinkQuery<T>>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Config<T> {
    pub fallback: FallbackBehavior,
    #[serde(flatten)]
    pub redirects: RedirectConfig<T>,
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
