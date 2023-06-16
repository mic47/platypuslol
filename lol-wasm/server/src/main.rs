use std::collections::{HashMap, HashSet};
use std::convert::Infallible;
use std::fmt::Write;
use std::net::SocketAddr;
use std::path::{Path, PathBuf};
use std::sync::{Arc, RwLock};

use anyhow::Context;
use html_builder::{Buffer, Html5};
use hyper::http::HeaderValue;
use hyper::service::{make_service_fn, service_fn};
use hyper::{Body, Method, Request, Response, Server, StatusCode};
use notify::{Event, RecursiveMode, Watcher};
use serde::{Deserialize, Serialize};

use nfa::NFA;
use redirect::{
    create_parser, resolve_parsed_output, resolve_suggestion_output, ConfigLinkQuery, LinkToken,
    QueryToken,
};

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Config {
    substitutions: HashMap<String, Vec<HashMap<String, String>>>,
    redirects: Vec<ConfigLinkQuery<String>>,
}

const INSTALL_INSTRUCTIONS: &str = include_str!("../../../resources/index.html");
const OPENSEARCH: &str = include_str!("../../../resources/opensearch.xml");
lazy_static::lazy_static! {
    static ref ICONS: HashMap<&'static str, Vec<u8>> = HashMap::from([
        ("/favicon-16x16.png", Vec::from(*include_bytes!("../../../resources/favicon-16x16.png"))),
        ("/favicon-32x32.png", Vec::from(*include_bytes!("../../../resources/favicon-32x32.png"))),
        ("/favicon-96x96.png", Vec::from(*include_bytes!("../../../resources/favicon-96x96.png"))),
        ("/favicon.ico", Vec::from(*include_bytes!("../../../resources/favicon.ico"))),
    ]);
}

#[derive(Clone, Debug, clap::Parser)]
struct Cli {
    #[arg(short, long, value_name = "FILE")]
    pub link_config: PathBuf,

    #[arg(short, long, value_name = "PORT", default_value = "3000")]
    pub port: u16,
}

async fn debug(
    req: Request<Body>,
    parser: Arc<NFA<(Vec<LinkToken>, Vec<QueryToken>)>>,
) -> anyhow::Result<Response<Body>> {
    let p = query_params(&req);
    if let Some(query) = p.get("q") {
        let (parsed, suggested) = parser.parse_full_and_suggest(query);
        let parsed: Vec<_> = parsed.into_iter().map(resolve_parsed_output).collect();
        let suggested: Vec<_> = suggested
            .into_iter()
            .map(resolve_suggestion_output)
            .collect();
        return to_string_response(
            serde_json::to_value((parsed, suggested)).context("Unable to serialize to json")?,
            ContentType::Json,
        );
    }
    Ok(not_found())
}

async fn suggest(
    req: Request<Body>,
    parser: Arc<NFA<(Vec<LinkToken>, Vec<QueryToken>)>>,
) -> anyhow::Result<Response<Body>> {
    let p = query_params(&req);
    if let Some(query) = p.get("q") {
        let (parsed, suggested) = parser.parse_full_and_suggest(query);
        let mut suggested_queries: Vec<String> = vec![];
        let mut suggested_texts: Vec<String> = vec![];
        let mut suggested_urls: Vec<String> = vec![];
        for p in parsed.into_iter().map(resolve_parsed_output) {
            suggested_texts.push(format!("{} => {}", p.description, p.link));
            suggested_queries.push(p.description);
            suggested_urls.push(p.link);
        }
        let mut visited: HashSet<_> = HashSet::default();
        for s in suggested.into_iter().map(resolve_suggestion_output) {
            if !visited.insert(s.clone()) {
                continue;
            }
            suggested_texts.push(format!(
                "{} ➡️  {}",
                s.description,
                s.link.clone().unwrap_or("???".into())
            ));
            suggested_queries.push(s.description);
            suggested_urls.push(s.link.unwrap_or("???".into()));
        }
        let response =
            serde_json::to_value((query, suggested_queries, suggested_texts, suggested_urls))
                .context("Unable to serialize suggestions")?;
        println!("{:?}", response);
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
        .unwrap_or_else(HashMap::new)
}

async fn redirect(
    req: Request<Body>,
    parser: Arc<NFA<(Vec<LinkToken>, Vec<QueryToken>)>>,
) -> anyhow::Result<Response<Body>> {
    let p = query_params(&req);
    if let Some(query) = p.get("q") {
        let (parsed, _) = parser.parse_full_and_suggest(query);
        if let Some(p) = parsed.into_iter().map(resolve_parsed_output).next() {
            return redirect_response(&p.link);
        }
    }
    // TODO: default if you can't find any suggestion?
    list(req, parser).await
}

async fn list(
    req: Request<Body>,
    parser: Arc<NFA<(Vec<LinkToken>, Vec<QueryToken>)>>,
) -> anyhow::Result<Response<Body>> {
    let p = query_params(&req);
    let (used_query, (parsed, suggested)) = p
        .get("q")
        .and_then(|q| {
            let (p, s) = parser.parse_full_and_suggest(q);
            if p.is_empty() && s.is_empty() {
                None
            } else {
                Some((Some(q), (p, s)))
            }
        })
        .unwrap_or_else(|| (None, parser.parse_full_and_suggest("")));
    let mut matches = vec![];
    for p in parsed.into_iter().map(resolve_parsed_output) {
        matches.push((p.description, Some(p.link)))
    }
    let mut visited: HashSet<_> = HashSet::default();
    for s in suggested.into_iter().map(resolve_suggestion_output) {
        if !visited.insert(s.clone()) {
            continue;
        }
        matches.push((s.description, s.link))
    }
    matches.sort();

    let mut buf = Buffer::new();
    let mut html = buf.html().attr("lang='en'");
    writeln!(html.head().title(), "List of platypus lol commands")?;
    if let Some(used_query) = used_query {
        writeln!(
            html.body().h1(),
            "List of Commands for Query '{}'",
            used_query
        )?;
    } else {
        writeln!(html.body().h1(), "List of All Commands")?;
    }
    let mut body = html.body();
    let mut list = body.ul();
    for (description, link) in matches.into_iter() {
        if let Some(link) = link {
            writeln!(
                // TODO: escape link?
                list.li().a().attr(&format!("href='{}'", link)),
                "{}",
                description,
            )?;
        } else {
            writeln!(list.li(), "{}", description,)?;
        }
    }
    to_string_response(buf.finish(), ContentType::Html)
}

#[allow(clippy::type_complexity)]
async fn route(
    req: Request<Body>,
    parser: Arc<RwLock<Arc<NFA<(Vec<LinkToken>, Vec<QueryToken>)>>>>,
    template_variables: HashMap<String, String>,
) -> Result<Response<Body>, Infallible> {
    let method = req.method();
    let path = req.uri().path();
    let path = path.strip_suffix('/').unwrap_or(path);
    let parser: Arc<NFA<_>> = { parser.read().unwrap().clone() };
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
        (&Method::GET, "/list") => list(req, parser).await,
        (&Method::GET, "/debug") => debug(req, parser).await,
        (&Method::GET, "/redirect") => redirect(req, parser).await,
        (&Method::GET, "/suggest") => suggest(req, parser).await,
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

fn redirect_response(uri: &str) -> anyhow::Result<Response<Body>> {
    let (mut parts, body) = Response::<Body>::default().into_parts();
    parts
        .headers
        .insert(hyper::header::LOCATION, HeaderValue::from_str(uri)?);
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

#[allow(clippy::type_complexity)]
fn load_parser(path: &Path) -> anyhow::Result<Arc<NFA<(Vec<LinkToken>, Vec<QueryToken>)>>> {
    // TODO: better tool
    let config: Config = serde_json::from_str(
        &std::fs::read_to_string(path)
            .with_context(|| format!("Unable to find config file {:?}", path))?,
    )
    .context("Unable to parse config.")?;
    Ok(Arc::new(
        create_parser(config.redirects, config.substitutions)
            .map_err(|err| anyhow::anyhow!("Unable to create parser {}", err))?,
    ))
}

#[allow(clippy::type_complexity)]
fn config_watcher(
    path: PathBuf,
    parser: Arc<RwLock<Arc<NFA<(Vec<LinkToken>, Vec<QueryToken>)>>>>,
) -> anyhow::Result<()> {
    let watcher_path = path.clone();
    let mut watcher = notify::recommended_watcher(move |res: Result<Event, _>| {
        let path = watcher_path.clone();
        match res {
            Ok(event) => {
                if event
                    .paths
                    .iter()
                    .any(|x| path.file_name().map(|p| x.ends_with(p)).unwrap_or(true))
                {
                    let new_parser = load_parser(&path);
                    match new_parser {
                        Ok(new_parser) => {
                            let mut parser = parser.write().unwrap();
                            *parser = new_parser;
                            println!("Config is reloaded {:?}", &path);
                        }
                        Err(err) => {
                            println!("Unable to load config: {err}");
                        }
                    };
                }
            }
            Err(e) => println!("watch error: {:?}", e),
        }
    })?;

    // Need to watch parent directory, as editors might remove this file and again add it, which
    // confuses watcher.
    let parent = path.parent().unwrap_or(&path);
    watcher
        .watch(parent, RecursiveMode::Recursive)
        .context("Unable to watch config file")
}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let cli = <Cli as clap::Parser>::parse();

    let parser = Arc::new(RwLock::new(load_parser(&cli.link_config)?));

    config_watcher(cli.link_config.clone(), parser.clone())?;
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
                route(req, parser, template_variables)
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
