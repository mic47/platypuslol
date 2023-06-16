use std::collections::{HashMap, HashSet};
use std::convert::Infallible;
use std::fmt::Write;
use std::net::SocketAddr;
use std::path::PathBuf;
use std::sync::Arc;

use html_builder::{Buffer, Html5};
use hyper::header::InvalidHeaderValue;
use hyper::http::HeaderValue;
use hyper::service::{make_service_fn, service_fn};
use hyper::{Body, Method, Request, Response, Server, StatusCode};
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

#[derive(clap::Parser)]
struct Cli {
    #[arg(short, long, value_name = "FILE")]
    pub link_config: PathBuf,

    #[arg(short, long, value_name = "PORT", default_value = "3000")]
    pub port: u16,
}

async fn debug(
    req: Request<Body>,
    parser: Arc<NFA<(Vec<LinkToken>, Vec<QueryToken>)>>,
) -> Response<Body> {
    let p = query_params(&req);
    if let Some(query) = p.get("q") {
        let (parsed, suggested) = parser.parse_full_and_suggest(query);
        let parsed: Vec<_> = parsed.into_iter().map(resolve_parsed_output).collect();
        let suggested: Vec<_> = suggested
            .into_iter()
            .map(resolve_suggestion_output)
            .collect();
        return to_string_response(
            serde_json::to_value((parsed, suggested)).unwrap(),
            ContentType::Json,
        );
    }
    not_found()
}

async fn suggest(
    req: Request<Body>,
    parser: Arc<NFA<(Vec<LinkToken>, Vec<QueryToken>)>>,
) -> Response<Body> {
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
                .unwrap();
        println!("{:?}", response);
        return to_string_response(response, ContentType::SuggestionsJson);
    }
    not_found()
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
) -> Response<Body> {
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
) -> Response<Body> {
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
    writeln!(html.head().title(), "List of platypus lol commands").unwrap();
    if let Some(used_query) = used_query {
        writeln!(
            html.body().h1(),
            "List of Commands for Query '{}'",
            used_query
        )
        .unwrap();
    } else {
        writeln!(html.body().h1(), "List of All Commands").unwrap();
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
            )
            .unwrap();
        } else {
            writeln!(list.li(), "{}", description,).unwrap();
        }
    }
    to_string_response(buf.finish(), ContentType::Html)
}

async fn route(
    req: Request<Body>,
    parser: Arc<NFA<(Vec<LinkToken>, Vec<QueryToken>)>>,
    template_variables: HashMap<String, String>,
) -> Result<Response<Body>, Infallible> {
    let method = req.method();
    let path = req.uri().path();
    let path = path.strip_suffix('/').unwrap_or(path);
    match (method, path) {
        (&Method::GET, "") | (&Method::GET, "/install") => Ok(templated_string_response(
            INSTALL_INSTRUCTIONS.into(),
            &template_variables,
            ContentType::Html,
        )),
        (&Method::GET, "/install/opensearch.xml") => Ok(templated_string_response(
            OPENSEARCH.into(),
            &template_variables,
            ContentType::OpenSearchDescription,
        )),
        (&Method::GET, "/list") => Ok(list(req, parser).await),
        (&Method::GET, "/debug") => Ok(debug(req, parser).await),
        (&Method::GET, "/redirect") => Ok(redirect(req, parser).await),
        (&Method::GET, "/suggest") => Ok(suggest(req, parser).await),
        (_, path) => {
            if let Some(icon) = ICONS.get(&path) {
                Ok(bytes_response(icon.clone(), ContentType::Png))
            } else {
                Ok(not_found())
            }
        }
    }
}

enum ContentType {
    Json,
    SuggestionsJson,
    Html,
    OpenSearchDescription,
    Png,
}

impl TryFrom<ContentType> for HeaderValue {
    type Error = InvalidHeaderValue;

    fn try_from(value: ContentType) -> Result<Self, Self::Error> {
        HeaderValue::from_str(match value {
            ContentType::Json => "application/json",
            ContentType::SuggestionsJson => "application/x-suggestions+json",
            ContentType::Html => "text/html",
            ContentType::OpenSearchDescription => "application/opensearchdescription+xml",
            ContentType::Png => "image/png",
        })
    }
}

fn to_string_response<T: ToString>(value: T, content_type: ContentType) -> Response<Body> {
    let (mut parts, _) = Response::<Body>::default().into_parts();
    parts.headers.insert(
        hyper::header::CONTENT_TYPE,
        HeaderValue::try_from(content_type).unwrap(),
    );
    let body = Body::from(value.to_string());
    Response::from_parts(parts, body)
}

fn bytes_response(value: Vec<u8>, content_type: ContentType) -> Response<Body> {
    let (mut parts, _) = Response::<Body>::default().into_parts();
    parts.headers.insert(
        hyper::header::CONTENT_TYPE,
        HeaderValue::try_from(content_type).unwrap(),
    );
    let body = Body::from(value);
    Response::from_parts(parts, body)
}

fn templated_string_response(
    value: String,
    replacements: &HashMap<String, String>,
    content_type: ContentType,
) -> Response<Body> {
    let (mut parts, _) = Response::<Body>::default().into_parts();
    parts.headers.insert(
        hyper::header::CONTENT_TYPE,
        HeaderValue::try_from(content_type).unwrap(),
    );
    let mut value = value;
    for (k, v) in replacements {
        value = value.replace(k, v);
    }
    let body = Body::from(value);
    Response::from_parts(parts, body)
}

fn redirect_response(uri: &str) -> Response<Body> {
    let (mut parts, body) = Response::<Body>::default().into_parts();
    parts
        .headers
        .insert(hyper::header::LOCATION, HeaderValue::from_str(uri).unwrap());
    parts.status = StatusCode::FOUND;
    Response::from_parts(parts, body)
}

fn not_found() -> Response<Body> {
    Response::builder()
        .status(StatusCode::NOT_FOUND)
        .body(Body::empty())
        .unwrap()
}

#[tokio::main]
async fn main() {
    // We'll bind to 127.0.0.1:3000

    let cli = <Cli as clap::Parser>::parse();

    let addr = SocketAddr::from(([127, 0, 0, 1], cli.port));

    // TODO: better tool
    let config: Config =
        serde_json::from_str(&std::fs::read_to_string(cli.link_config).unwrap()).unwrap();
    let parser = Arc::new(create_parser(config.redirects, config.substitutions).unwrap());
    let default_server = format!("localhost:{}", cli.port);
    let default_protocol = "http://";

    // A `Service` is needed for every connection, so this
    // creates one from our `hello_world` function.
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
                        .map(|x| x.to_str().unwrap().into())
                        .unwrap_or(default_server)
                );
                let template_variables = HashMap::from([("{server}".into(), server_uri)]);
                route(req, parser, template_variables)
            }))
        }
    });

    let server = Server::bind(&addr).serve(make_svc);

    // Run this server for... forever!
    if let Err(e) = server.await {
        eprintln!("server error: {}", e);
    }
}
