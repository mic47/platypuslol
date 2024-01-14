use std::collections::{HashMap, HashSet};
use std::convert::Infallible;
use std::fmt::Write;
use std::net::SocketAddr;
use std::path::{Path, PathBuf};
use std::sync::{Arc, RwLock};

use anyhow::Context;
use html_builder::{Buffer, Html5};
use hyper::body::Buf;
use hyper::http::HeaderValue;
use hyper::service::{make_service_fn, service_fn};
use hyper::{Body, Client, Method, Request, Response, Server, StatusCode, Uri};
use notify::{Event, INotifyWatcher, RecursiveMode, Watcher};

use redirect::{
    resolve_parsed_output, resolve_suggestion_output, CommonAppState, Config, ConfigUrl,
    ExternalParser, RedirectConfig,
};
use tokio::runtime::Runtime;

const INSTALL_INSTRUCTIONS: &str = include_str!("../../resources/index.html");
const OPENSEARCH: &str = include_str!("../../resources/opensearch.xml");
lazy_static::lazy_static! {
    static ref ICONS: HashMap<&'static str, Vec<u8>> = HashMap::from([
        ("/favicon-16x16.png", Vec::from(*include_bytes!("../../resources/favicon-16x16.png"))),
        ("/favicon-32x32.png", Vec::from(*include_bytes!("../../resources/favicon-32x32.png"))),
        ("/favicon-96x96.png", Vec::from(*include_bytes!("../../resources/favicon-96x96.png"))),
        ("/favicon.ico", Vec::from(*include_bytes!("../../resources/favicon.ico"))),
    ]);

    static ref BUILTIN_PARSERS: HashMap<String, &'static str> = HashMap::from([
        ("commands/base.json".into(), include_str!("../../extension/commands/base.json")),
        ("commands/google.json".into(), include_str!("../../extension/commands/google.json")),
        ("commands/python.json".into(), include_str!("../../extension/commands/python.json")),
        ("commands/github.json".into(), include_str!("../../extension/commands/github.json")),
    ]);
}
const LIST_JS: &str = include_str!("../../resources/keypress.js");

#[derive(Clone, Debug, clap::Parser)]
struct Cli {
    #[arg(short, long, value_name = "FILE")]
    pub link_config: PathBuf,

    #[arg(short, long, value_name = "PORT", default_value = "3047")]
    pub port: u16,
}

fn debug(req: Request<Body>, state: Arc<CommonAppState>) -> anyhow::Result<Response<Body>> {
    let p = query_params(&req);
    if let Some(query) = p.get("q") {
        let (parsed, suggested) = state.parser.parse_full_and_suggest(query);
        let parsed: Vec<_> = parsed
            .into_iter()
            .map(|x| resolve_parsed_output(x, &None))
            .collect();
        let suggested: Vec<_> = suggested
            .into_iter()
            .map(|x| resolve_suggestion_output(x, &None))
            .collect();
        return to_string_response(
            serde_json::to_value((parsed, suggested)).context("Unable to serialize to json")?,
            ContentType::Json,
        );
    }
    Ok(not_found())
}

fn suggest(req: Request<Body>, state: Arc<CommonAppState>) -> anyhow::Result<Response<Body>> {
    let p = query_params(&req);
    let mut suggestions_left: i32 = 20; // TODO: configure
    if let Some(query) = p.get("q") {
        let (parsed, suggested) = state.parser.parse_full_and_suggest(query);
        let mut suggested_queries: Vec<String> = vec![];
        let mut suggested_texts: Vec<String> = vec![];
        let mut suggested_urls: Vec<String> = vec![];
        for p in parsed.into_iter().map(|x| resolve_parsed_output(x, &None)) {
            suggested_texts.push(format!("{} => {:?}", p.description, p.links));
            suggested_queries.push(p.description.clone());
            if let [link] = &p.links[..] {
                suggested_urls.push(link.clone());
            } else {
                suggested_urls.push(format!("redirect?q={}", p.description));
            }
            suggestions_left -= 1;
            if suggestions_left <= 0 {
                break;
            }
        }
        let mut visited: HashSet<_> = HashSet::default();
        for s in suggested
            .into_iter()
            .map(|x| resolve_suggestion_output(x, &None))
        {
            if !visited.insert(s.clone()) {
                continue;
            }
            suggested_texts.push(format!(
                "{} ➡️  {:?}",
                s.description,
                s.links.as_ref().unwrap_or(&vec!["???".into()])
            ));
            suggested_queries.push(s.description.clone());
            if let [link] = &s.links.unwrap_or(vec!["???".into()])[..] {
                suggested_urls.push(link.clone());
            } else {
                suggested_urls.push(format!("redirect?q={}", s.description));
            }
            suggestions_left -= 1;
            if suggestions_left <= 0 {
                break;
            }
        }
        let response =
            serde_json::to_value((query, suggested_queries, suggested_texts, suggested_urls))
                .context("Unable to serialize suggestions")?;
        return to_string_response(response, ContentType::SuggestionsJson);
    }
    Ok(not_found())
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

fn redirect(req: Request<Body>, state: Arc<CommonAppState>) -> anyhow::Result<Response<Body>> {
    let p = query_params(&req);
    if let Some(query) = p.get("q") {
        if let Some(link) = state.redirect(query) {
            return redirect_response(&link);
        }
        return list(req, state, Some(query));
    }
    list(req, state, None)
}

fn local(req: Request<Body>, state: Arc<CommonAppState>) -> anyhow::Result<Response<Body>> {
    let p = query_params(&req);
    if let Some(file) = p.get("file") {
        match state.local_configs.get(file) {
            Some(content) => return to_string_response(content, ContentType::Json),
            None => Err(anyhow::anyhow!("Missing file {}", file))?,
        }
    }
    Err(anyhow::anyhow!("Missing parameter file"))
}

fn add_key_class(class: Option<String>, item: html_builder::Node) -> html_builder::Node {
    if let Some(class) = class {
        item.attr(&format!("class='{}'", class))
    } else {
        item
    }
}

fn list(
    req: Request<Body>,
    state: Arc<CommonAppState>,
    failed_query: Option<&str>,
) -> anyhow::Result<Response<Body>> {
    let mut failed_matches = vec![];
    if let Some(failed_query) = failed_query {
        let (parsed, _) = state
            .fallback
            .parser
            .parse_full_and_suggest(&state.fallback.behavior.make_query(failed_query));
        if let Some(p) = parsed
            .into_iter()
            .map(|x| resolve_parsed_output(x, &None))
            .next()
        {
            failed_matches.push((p.description, Some(p.links), Some(p.score)));
        }
    }
    let mut available_key_classes = "fjdkslahgrueiwoqptyvncmxbz1234567890"
        .chars()
        .map(|c| (format!("[{}] ", c), format!("onpress{}", c)));

    let p = query_params(&req);
    let (used_query, (parsed, suggested)) = p
        .get("q")
        .and_then(|q| {
            let (p, s) = state.parser.parse_full_and_suggest(q);
            if p.is_empty() && s.is_empty() {
                None
            } else {
                Some((Some(q), (p, s)))
            }
        })
        .unwrap_or_else(|| (None, state.parser.parse_full_and_suggest("")));
    let mut matches = vec![];
    for p in parsed
        .into_iter()
        .map(|x| resolve_parsed_output(x, &failed_query.map(Into::into)))
    {
        matches.push((p.description, Some(p.links), Some(p.score)))
    }
    let mut visited: HashSet<_> = HashSet::default();
    for s in suggested
        .into_iter()
        .map(|x| resolve_suggestion_output(x, &failed_query.map(Into::into)))
    {
        if !visited.insert(s.clone()) {
            continue;
        }
        matches.push((s.description, s.links, None))
    }
    matches.sort_by_key(|(x0, x1, _)| (x0.clone(), x1.clone()));

    let mut buf = Buffer::new();
    let mut html = buf.html().attr("lang='en'");
    let mut head = html.head();
    writeln!(head.title(), "List of platypus lol commands")?;
    writeln!(head.script().raw(), "{}", LIST_JS)?;
    let mut body = html.body().attr("onload='onLoad()'");
    if let Some(used_query) = used_query {
        writeln!(body.h1(), "List of Commands for Query '{}'", used_query)?;
    } else {
        writeln!(body.h1(), "List of All Commands")?;
    }
    let mut list = body.ul();
    for (description, links, score) in failed_matches.into_iter().chain(matches.into_iter()) {
        if let Some(links) = links {
            if let [link] = &links[..] {
                let maybe_key = available_key_classes.next();
                writeln!(
                    // TODO: escape link?
                    add_key_class(
                        maybe_key.clone().map(|x| x.1),
                        list.li().a().attr(&format!("href='{}'", link))
                    ),
                    "{}{}{}",
                    maybe_key.map(|x| x.0).unwrap_or_default(),
                    description,
                    score
                        .map(|score| format!(" {{{score}}}"))
                        .unwrap_or_default(),
                )?;
            } else {
                let mut li = list.li();
                let mut div = li.span();
                let maybe_key = available_key_classes.next();
                writeln!(
                    div,
                    "{}{}{}",
                    maybe_key.clone().map(|x| x.0).unwrap_or_default(),
                    description,
                    score
                        .map(|score| format!(" {{{score}}}"))
                        .unwrap_or_default(),
                )?;
                let mut ul = div.ul();
                for link in links {
                    writeln!(
                        // TODO: escape link?
                        add_key_class(
                            maybe_key.clone().map(|x| x.1),
                            ul.li().a().attr(&format!("href='{}'", link))
                        ),
                        "{}",
                        link,
                    )?;
                }
            }
        } else {
            writeln!(
                list.li(),
                "{}{}",
                description,
                score
                    .map(|score| format!(" {{{score}}}"))
                    .unwrap_or_default(),
            )?;
        }
    }
    to_string_response(buf.finish(), ContentType::Html)
}

fn route(
    req: Request<Body>,
    state: Arc<RwLock<Arc<CommonAppState>>>,
    template_variables: HashMap<String, String>,
) -> Result<Response<Body>, Infallible> {
    let method = req.method();
    let path = req.uri().path();
    let path = path.strip_suffix('/').unwrap_or(path);
    let state: Arc<CommonAppState> = { state.read().unwrap().clone() };
    Ok((match (method, path) {
        (&Method::GET, "") | (&Method::GET, "/install") => templated_string_response(
            INSTALL_INSTRUCTIONS.into(),
            &template_variables,
            ContentType::Html,
        ),
        (&Method::GET, "/install/opensearch.xml") => templated_string_response(
            OPENSEARCH.into(),
            &template_variables,
            ContentType::OpenSearchDescription,
        ),
        (&Method::GET, "/list") => list(req, state, None),
        (&Method::GET, "/debug") => debug(req, state),
        (&Method::GET, "/redirect") => redirect(req, state),
        (&Method::GET, "/config") => local(req, state),
        (&Method::GET, "/suggest") => suggest(req, state),
        (_, path) => {
            if let Some(icon) = ICONS.get(&path) {
                bytes_response(icon.clone(), ContentType::Png)
            } else {
                Ok(not_found())
            }
        }
    })
    .unwrap_or_else(internal_server_error))
}

enum ContentType {
    Json,
    SuggestionsJson,
    Html,
    OpenSearchDescription,
    Png,
}

impl TryFrom<ContentType> for HeaderValue {
    type Error = anyhow::Error;

    fn try_from(value: ContentType) -> Result<Self, Self::Error> {
        HeaderValue::from_str(match value {
            ContentType::Json => "application/json",
            ContentType::SuggestionsJson => "application/x-suggestions+json",
            ContentType::Html => "text/html",
            ContentType::OpenSearchDescription => "application/opensearchdescription+xml",
            ContentType::Png => "image/png",
        })
        .context("Unable to convert redirect to location")
    }
}

fn to_string_response<T: ToString>(
    value: T,
    content_type: ContentType,
) -> anyhow::Result<Response<Body>> {
    let (mut parts, _) = Response::<Body>::default().into_parts();
    parts.headers.insert(
        hyper::header::CONTENT_TYPE,
        HeaderValue::try_from(content_type)?,
    );
    let body = Body::from(value.to_string());
    Ok(Response::from_parts(parts, body))
}

fn bytes_response(value: Vec<u8>, content_type: ContentType) -> anyhow::Result<Response<Body>> {
    let (mut parts, _) = Response::<Body>::default().into_parts();
    parts.headers.insert(
        hyper::header::CONTENT_TYPE,
        HeaderValue::try_from(content_type)?,
    );
    let body = Body::from(value);
    Ok(Response::from_parts(parts, body))
}

fn templated_string_response(
    value: String,
    replacements: &HashMap<String, String>,
    content_type: ContentType,
) -> anyhow::Result<Response<Body>> {
    let (mut parts, _) = Response::<Body>::default().into_parts();
    parts.headers.insert(
        hyper::header::CONTENT_TYPE,
        HeaderValue::try_from(content_type)?,
    );
    let mut value = value;
    for (k, v) in replacements {
        value = value.replace(k, v);
    }
    let body = Body::from(value);
    Ok(Response::from_parts(parts, body))
}

fn redirect_multi(links: &[String]) -> anyhow::Result<String> {
    Ok(format!(
        "<html>
<head>
  <script>
function redirectLol() {{
  var data = {};
  for (var i = 1; i < data.length ; i++) {{
    window.open(data[i], '_blank');
  }};
  window.location.href = data[0];
}}
  </script>
</head>
<body onload='redirectLol()'>
<body>
</html>",
        serde_json::to_string(links).context("Unable to convert links to string")?,
    ))
}

fn redirect_response(uris: &[String]) -> anyhow::Result<Response<Body>> {
    if let [uri] = uris {
        let (mut parts, body) = Response::<Body>::default().into_parts();
        parts
            .headers
            .insert(hyper::header::LOCATION, HeaderValue::from_str(uri)?);
        parts.status = StatusCode::FOUND;
        Ok(Response::from_parts(parts, body))
    } else {
        to_string_response(redirect_multi(uris)?, ContentType::Html)
    }
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

async fn load_fetch_and_parse_configs(
    config_path: &Path,
) -> anyhow::Result<(
    Config<String, RedirectConfig<String>>,
    HashMap<String, String>,
)> {
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
    let local_configs = external_configurations
        .iter()
        .filter_map(|(url, _)| match url.clone() {
            ConfigUrl::Local { path } => {
                let parent = config_path.parent().unwrap_or(config_path);
                std::fs::read_to_string(parent.join(&path))
                    .ok()
                    .map(|content| (path.clone(), content))
            }
            _ => None,
        })
        .collect::<HashMap<_, _>>();
    let external_configurations: anyhow::Result<HashMap<_, _>> = futures_util::future::join_all(
        external_configurations
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
            .map(|(url, redirect_config)| async move {
                let config = if redirect_config.enabled {
                    match url.clone() {
                        ConfigUrl::Builtin { path } => {
                            let ret: RedirectConfig<String> = RedirectConfig::from_config_file(
                                serde_json::from_str(BUILTIN_PARSERS.get(&path).with_context(
                                    || format!("Unable to find builtin config {path}"),
                                )?)
                                .with_context(|| {
                                    format!("Unable to parse builtin config {path}")
                                })?,
                            )
                            .map_err(anyhow::Error::msg)?;
                            Some(ret)
                        }
                        ConfigUrl::Local { path } => {
                            let parent = config_path.parent().unwrap_or(config_path);
                            let content = std::fs::read_to_string(parent.join(&path))
                                .with_context(|| {
                                    format!("Unable to find config file {:?}", path)
                                })?;
                            let config: RedirectConfig<String> = RedirectConfig::from_config_file(
                                serde_json::from_str(&content)
                                    .with_context(|| format!("Unable to parse config {path}"))?,
                            )
                            .map_err(anyhow::Error::msg)?;
                            Some(config)
                        }
                        ConfigUrl::Remote { url } => {
                            let fun = || async {
                                let client = Client::new();
                                let res =
                                    client
                                        .get(url.parse::<Uri>().with_context(|| {
                                            format!("Unable to parse url {}", url)
                                        })?)
                                        .await?;
                                if !res.status().is_success() {
                                    Err(anyhow::anyhow!(
                                        "Received bas status {:?} when processing {}",
                                        res.status(),
                                        url
                                    ))?
                                }
                                let body = hyper::body::aggregate(res).await.context("TODO")?;
                                let config: RedirectConfig<String> =
                                    RedirectConfig::from_config_file(
                                        serde_json::from_reader(body.reader()).context("TODO")?,
                                    )
                                    .map_err(anyhow::Error::msg)?;
                                anyhow::Ok(config)
                            };
                            fun()
                                .await
                                .map_err(|err| {
                                    eprintln!("Unable to fetch & parse external config {err}")
                                })
                                .ok()
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
            }),
    )
    .await
    .into_iter()
    .collect();
    Ok((
        Config {
            fallback,
            behavior,
            redirects,
            external_configurations: external_configurations?,
        },
        local_configs,
    ))
}

async fn load_config(config_path: &Path) -> anyhow::Result<Arc<CommonAppState>> {
    load_fetch_and_parse_configs(config_path)
        .await
        .and_then(|(x, local_configs)| {
            CommonAppState::new(x, local_configs)
                .map_err(|err| anyhow::anyhow!("Unable to create state: {err}"))
        })
        .map(Arc::new)
}

fn config_watcher(
    path: PathBuf,
    state: Arc<RwLock<Arc<CommonAppState>>>,
) -> anyhow::Result<INotifyWatcher> {
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
                        .map(|x| {
                            let new_parser = x.block_on(load_config(&path));
                            match new_parser {
                                Ok(new_parser) => {
                                    let mut parser = state.write().unwrap();
                                    *parser = new_parser;
                                    eprintln!("Config is reloaded {:?}", &path);
                                }
                                Err(err) => {
                                    eprintln!("Unable to load config: {err}");
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

    let parser = Arc::new(RwLock::new(load_config(&cli.link_config).await?));

    let _watcher = config_watcher(cli.link_config.clone(), parser.clone())?;
    let default_server = format!("localhost:{}", cli.port);
    let default_protocol = "http://";

    let make_svc = make_service_fn(move |_conn| {
        let parser = parser.clone();
        let default_server = default_server.clone();
        async move {
            Ok::<_, Infallible>(service_fn(move |req| {
                let parser = parser.clone();
                let default_server = default_server.clone();
                let server_uri = format!(
                    "{default_protocol}{}",
                    req.headers()
                        .get("Host")
                        .and_then(|x| x.to_str().ok())
                        .unwrap_or(&default_server)
                );
                let template_variables = HashMap::from([("{server}".into(), server_uri)]);
                async { route(req, parser, template_variables) }
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
