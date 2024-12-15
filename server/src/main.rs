use std::collections::HashMap;
use std::convert::Infallible;
use std::net::SocketAddr;
use std::path::{Path, PathBuf};
use std::sync::{Arc, RwLock};

use anyhow::Context;
use hyper::http::HeaderValue;
use hyper::service::{make_service_fn, service_fn};
use hyper::{Body, Method, Request, Response, Server, StatusCode};
use notify::{Event, RecommendedWatcher, RecursiveMode, Watcher};

use redirect::{CommonAppState, Config, ConfigUrl, ExternalParser, RedirectConfig};
use tokio::runtime::Runtime;
use web_redirector::{route_get, ContentType, LastParsingError, ServerResponse};

lazy_static::lazy_static! {

    static ref BUILTIN_PARSERS: HashMap<String, &'static str> = HashMap::from([
        ("commands/base.json".into(), include_str!("../../config/commands/base.json")),
        ("commands/google.json".into(), include_str!("../../config/commands/google.json")),
        ("commands/python.json".into(), include_str!("../../config/commands/python.json")),
        ("commands/github.json".into(), include_str!("../../config/commands/github.json")),
        ("commands/mic.json".into(), include_str!("../../config/commands/mic.json")),
    ]);
}
#[derive(Clone, Debug, clap::Parser)]
struct Cli {
    #[arg(short, long, value_name = "FILE")]
    pub link_config: PathBuf,

    #[arg(short, long, value_name = "PORT", default_value = "3047")]
    pub port: u16,
}

fn query_params(req: &Request<Body>) -> HashMap<String, String> {
    req.uri()
        .query()
        .map(|v| {
            url::form_urlencoded::parse(v.as_bytes())
                .into_owned()
                .collect()
        })
        .unwrap_or_default()
}

fn route(
    req: Request<Body>,
    state: Arc<RwLock<Arc<CommonAppState>>>,
    last_parsing_error: Arc<RwLock<LastParsingError>>,
    server_uri: String,
) -> Result<Response<Body>, Infallible> {
    let method = req.method();
    let path = req.uri().path();
    let path = path.strip_suffix('/').unwrap_or(path);
    let state: Arc<CommonAppState> = { state.read().unwrap().clone() };
    let last_parsing_error: LastParsingError = { last_parsing_error.read().unwrap().clone() };
    let req_query_params = query_params(&req);
    Ok((match method {
        &Method::GET => route_get(
            state,
            path,
            req_query_params,
            last_parsing_error,
            server_uri,
        )
        .and_then(|x| match x {
            ServerResponse::NotFound => Ok(not_found()),
            ServerResponse::Json(value) => to_string_response(value, ContentType::Json),
            ServerResponse::SuggestionsJson(value) => {
                to_string_response(value, ContentType::SuggestionsJson)
            }
            ServerResponse::Html(value) => to_string_response(value, ContentType::Html),
            ServerResponse::OpenSearchDescription(value) => {
                to_string_response(value, ContentType::OpenSearchDescription)
            }
            ServerResponse::Bytes { data, content_type } => bytes_response(data, content_type),
            ServerResponse::RedirectResponse { target } => redirect_response(target),
        }),
        _ => Ok(not_found()),
    })
    .unwrap_or_else(internal_server_error))
}

fn from_content_type(value: ContentType) -> anyhow::Result<HeaderValue> {
    HeaderValue::from_str(match value {
        ContentType::Json => "application/json",
        ContentType::SuggestionsJson => "application/x-suggestions+json",
        ContentType::Html => "text/html",
        ContentType::OpenSearchDescription => "application/opensearchdescription+xml",
        ContentType::Png => "image/png",
    })
    .context("Unable to convert redirect to location")
}

fn to_string_response<T: ToString>(
    value: T,
    content_type: ContentType,
) -> anyhow::Result<Response<Body>> {
    let (mut parts, _) = Response::<Body>::default().into_parts();
    parts.headers.insert(
        hyper::header::CONTENT_TYPE,
        from_content_type(content_type)?,
    );
    let body = Body::from(value.to_string());
    Ok(Response::from_parts(parts, body))
}

fn bytes_response(value: Vec<u8>, content_type: ContentType) -> anyhow::Result<Response<Body>> {
    let (mut parts, _) = Response::<Body>::default().into_parts();
    parts.headers.insert(
        hyper::header::CONTENT_TYPE,
        from_content_type(content_type)?,
    );
    let body = Body::from(value);
    Ok(Response::from_parts(parts, body))
}

fn redirect_response(target: String) -> anyhow::Result<Response<Body>> {
    let (mut parts, body) = Response::<Body>::default().into_parts();
    parts
        .headers
        .insert(hyper::header::LOCATION, HeaderValue::from_str(&target)?);
    parts.status = StatusCode::FOUND;
    Ok(Response::from_parts(parts, body))
}

fn not_found() -> Response<Body> {
    let (mut parts, body) = Response::<Body>::default().into_parts();
    parts.status = StatusCode::NOT_FOUND;
    Response::from_parts(parts, body)
}

fn internal_server_error<T: ToString>(error: T) -> Response<Body> {
    let (mut parts, _) = Response::<Body>::default().into_parts();
    parts.status = StatusCode::INTERNAL_SERVER_ERROR;
    let body = Body::from(error.to_string());
    Response::from_parts(parts, body)
}

fn load_fetch_and_parse_configs(
    config_path: &Path,
) -> anyhow::Result<Config<String, RedirectConfig<String>>> {
    // TODO: make sure error messages have line numbers / serde path
    let Config::<String, ()> {
        fallback,
        behavior,
        redirects,
        external_configurations,
    } = Config::<String, ()>::from_config_file(
        serde_json::from_str(
            &std::fs::read_to_string(config_path)
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
                        let content = std::fs::read_to_string(parent.join(&path))
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

fn load_config(config_path: &Path) -> anyhow::Result<Arc<CommonAppState>> {
    load_fetch_and_parse_configs(config_path)
        .and_then(|x| CommonAppState::new(x).context("Unable to create state"))
        .map(Arc::new)
}

fn config_watcher(
    path: PathBuf,
    state: Arc<RwLock<Arc<CommonAppState>>>,
    last_parsing_error: Arc<RwLock<LastParsingError>>,
) -> anyhow::Result<RecommendedWatcher> {
    let watcher_path = path.clone();
    let mut watcher = notify::recommended_watcher(move |res: Result<Event, _>| {
        let path = watcher_path.clone();
        match res {
            Ok(event) => {
                if event
                    .paths
                    .iter()
                    .any(|x| x.extension().map(|x| x == "json").unwrap_or(false))
                {
                    Runtime::new()
                        .map(|_| {
                            let new_parser = load_config(&path);
                            match new_parser {
                                Ok(new_parser) => {
                                    let mut parser = state.write().unwrap();
                                    *parser = new_parser;
                                    let mut last_error = last_parsing_error.write().unwrap();
                                    *last_error = Arc::new(None);
                                    eprintln!("Config is reloaded {:?}", &path);
                                }
                                Err(err) => {
                                    let mut last_error = last_parsing_error.write().unwrap();
                                    eprintln!("Unable to load config: {err}");
                                    *last_error = Arc::new(Some(err));
                                }
                            };
                        })
                        .map_err(|err| {
                            eprintln!("Unable to create tokyo runtime to load parser: {err}");
                        })
                        .ok();
                }
            }
            Err(e) => eprintln!("watch error: {:?}", e),
        }
    })?;

    // Need to watch parent directory, as editors might remove this file and again add it, which
    // confuses watcher.
    let parent = path.parent().unwrap_or(&path);
    watcher
        .watch(parent, RecursiveMode::Recursive)
        .context("Unable to watch config file")?;
    Ok(watcher)
}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let cli = <Cli as clap::Parser>::parse();
    eprintln!("Starting with following parameters {:#?}", cli);

    let parser = Arc::new(RwLock::new(load_config(&cli.link_config)?));
    let last_parsing_error = Arc::new(RwLock::new(Arc::new(None)));

    let _watcher = config_watcher(
        cli.link_config.clone(),
        parser.clone(),
        last_parsing_error.clone(),
    )?;
    let default_server = format!("localhost:{}", cli.port);
    let default_protocol = "http://";

    let make_svc = make_service_fn(move |_conn| {
        let parser = parser.clone();
        let last_parsing_error = last_parsing_error.clone();
        let default_server = default_server.clone();
        async move {
            Ok::<_, Infallible>(service_fn(move |req| {
                let parser = parser.clone();
                let last_parsing_error = last_parsing_error.clone();
                let default_server = default_server.clone();
                let server_uri = format!(
                    "{default_protocol}{}",
                    req.headers()
                        .get("Host")
                        .and_then(|x| x.to_str().ok())
                        .unwrap_or(&default_server)
                );
                async { route(req, parser, last_parsing_error, server_uri) }
            }))
        }
    });

    let addr = SocketAddr::from(([127, 0, 0, 1], cli.port));
    let server = Server::bind(&addr).serve(make_svc);

    if let Err(e) = server.await {
        eprintln!("server error: {}", e);
    }
    Ok(())
}
