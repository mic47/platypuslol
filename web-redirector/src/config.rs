use std::{collections::HashMap, path::Path, sync::Arc};

use anyhow::Context;
use redirect::{CommonAppState, Config, ConfigUrl, ExternalParser, RedirectConfig};

lazy_static::lazy_static! {

    static ref BUILTIN_PARSERS: HashMap<String, &'static str> = HashMap::from([
        ("commands/base.json".into(), include_str!("../../config/commands/base.json")),
        ("commands/google.json".into(), include_str!("../../config/commands/google.json")),
        ("commands/python.json".into(), include_str!("../../config/commands/python.json")),
        ("commands/github.json".into(), include_str!("../../config/commands/github.json")),
        ("commands/mic.json".into(), include_str!("../../config/commands/mic.json")),
    ]);
}

pub fn load_config<F: Fn(&Path) -> anyhow::Result<String>>(
    config_path: &Path,
    read_to_string: F,
) -> anyhow::Result<Arc<CommonAppState>> {
    load_fetch_and_parse_configs(config_path, read_to_string)
        .and_then(|x| CommonAppState::new(x).context("Unable to create state"))
        .map(Arc::new)
}

fn load_fetch_and_parse_configs<F: Fn(&Path) -> anyhow::Result<String>>(
    config_path: &Path,
    read_to_string: F,
) -> anyhow::Result<Config<String, RedirectConfig<String>>> {
    // TODO: make sure error messages have line numbers / serde path
    let Config::<String, ()> {
        fallback,
        behavior,
        redirects,
        external_configurations,
    } = Config::<String, ()>::from_config_file(
        serde_json::from_str(
            &read_to_string(config_path)
                .with_context(|| format!("Unable to find config file {:?}", config_path))?,
        )
        .context("Unable to parse config.")?,
    )
    .map_err(anyhow::Error::msg)?;
    let external_configurations: anyhow::Result<HashMap<_, _>> = external_configurations
        .into_iter()
        .chain([(
            ConfigUrl::Builtin {
                path: "commands/base.json".into(),
            },
            ExternalParser::<()> {
                enabled: true,
                substitutions_to_inherit: vec![],
                prefix: None,
                config: None,
            },
        )])
        .map(|(url, redirect_config)| {
            let config = if redirect_config.enabled {
                match url.clone() {
                    ConfigUrl::Builtin { path } => {
                        let ret: RedirectConfig<String> = RedirectConfig::from_config_file(
                            serde_json::from_str(BUILTIN_PARSERS.get(&path).with_context(
                                || format!("Unable to find builtin config {path}"),
                            )?)
                            .with_context(|| format!("Unable to parse builtin config {path}"))?,
                        )
                        .map_err(anyhow::Error::msg)?;
                        Some(ret)
                    }
                    ConfigUrl::Local { path } => {
                        let parent = config_path.parent().unwrap_or(config_path);
                        let content = read_to_string(&parent.join(&path))
                            .with_context(|| format!("Unable to find config file {:?}", path))?;
                        let config: RedirectConfig<String> = RedirectConfig::from_config_file(
                            serde_json::from_str(&content)
                                .with_context(|| format!("Unable to parse config {path}"))?,
                        )
                        .map_err(anyhow::Error::msg)?;
                        Some(config)
                    }
                }
            } else {
                None
            };
            let ExternalParser {
                enabled,
                substitutions_to_inherit,
                prefix,
                config: _,
            } = redirect_config;
            Ok((
                url,
                ExternalParser {
                    enabled,
                    substitutions_to_inherit,
                    prefix,
                    config,
                },
            ))
        })
        .collect();
    Ok(Config {
        fallback,
        behavior,
        redirects,
        external_configurations: external_configurations?,
    })
}
