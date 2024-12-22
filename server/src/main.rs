use std::convert::Infallible;
use std::net::SocketAddr;
use std::path::PathBuf;
use std::sync::{Arc, RwLock};

use anyhow::Context;
use http_body_util::Full;
use hyper::body::{Body, Bytes, Incoming};
use hyper::http::HeaderValue;
use hyper::server::conn::http1;
use hyper::service::service_fn;
use hyper::{Method, Request, Response, StatusCode};
use hyper_util::rt::TokioIo;
use notify::{Event, RecommendedWatcher, RecursiveMode, Watcher};

use redirect::CommonAppState;
use tokio::net::TcpListener;
use tokio::runtime::Runtime;
use web_redirector::{
    load_config, query_params, route_get, ContentType, LastParsingError, ServerResponse,
};

#[derive(Clone, Debug, clap::Parser)]
struct Cli {
    #[arg(short, long, value_name = "FILE")]
    pub link_config: PathBuf,

    #[arg(short, long, value_name = "PORT", default_value = "3047")]
    pub port: u16,
}

fn route<T: Body>(
    req: Request<T>,
    state: Arc<RwLock<Arc<CommonAppState>>>,
    last_parsing_error: Arc<RwLock<LastParsingError>>,
    server_uri: String,
) -> Result<Response<Full<Bytes>>, Infallible> {
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
    .context("Unable ko convert redirect to location")
}

fn to_string_response<T: ToString>(
    value: T,
    content_type: ContentType,
) -> anyhow::Result<Response<Full<Bytes>>> {
    let (mut parts, _) = Response::<Full<Bytes>>::default().into_parts();
    parts.headers.insert(
        hyper::header::CONTENT_TYPE,
        from_content_type(content_type)?,
    );
    let body = Full::new(Bytes::from(value.to_string()));
    Ok(Response::from_parts(parts, body))
}

fn bytes_response(
    value: Vec<u8>,
    content_type: ContentType,
) -> anyhow::Result<Response<Full<Bytes>>> {
    let (mut parts, _) = Response::<Full<Bytes>>::default().into_parts();
    parts.headers.insert(
        hyper::header::CONTENT_TYPE,
        from_content_type(content_type)?,
    );
    let body = Full::new(Bytes::from(value));
    Ok(Response::from_parts(parts, body))
}

fn redirect_response(target: String) -> anyhow::Result<Response<Full<Bytes>>> {
    let (mut parts, body) = Response::<Full<Bytes>>::default().into_parts();
    parts
        .headers
        .insert(hyper::header::LOCATION, HeaderValue::from_str(&target)?);
    parts.status = StatusCode::FOUND;
    Ok(Response::from_parts(parts, body))
}

fn not_found() -> Response<Full<Bytes>> {
    let (mut parts, body) = Response::<Full<Bytes>>::default().into_parts();
    parts.status = StatusCode::NOT_FOUND;
    Response::from_parts(parts, body)
}

fn internal_server_error<T: ToString>(error: T) -> Response<Full<Bytes>> {
    let (mut parts, _) = Response::<Full<Bytes>>::default().into_parts();
    parts.status = StatusCode::INTERNAL_SERVER_ERROR;
    let body = Full::new(Bytes::from(error.to_string()));
    Response::from_parts(parts, body)
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
                            let new_parser =
                                load_config(&path, |x| Ok(std::fs::read_to_string(x)?));
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

    let parser = Arc::new(RwLock::new(load_config(&cli.link_config, |x| {
        Ok(std::fs::read_to_string(x)?)
    })?));
    let last_parsing_error = Arc::new(RwLock::new(Arc::new(None)));

    let _watcher = config_watcher(
        cli.link_config.clone(),
        parser.clone(),
        last_parsing_error.clone(),
    )?;
    let default_server = format!("localhost:{}", cli.port);
    let default_protocol = "http://";

    let addr = SocketAddr::from(([127, 0, 0, 1], cli.port));
    let listener = TcpListener::bind(addr).await?;
    loop {
        let (stream, _) = listener.accept().await?;
        let io = TokioIo::new(stream);
        let parser = parser.clone();
        let last_parsing_error = last_parsing_error.clone();
        let default_server = default_server.clone();
        let serve = move |req: Request<Incoming>| {
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
        };
        tokio::task::spawn(async move {
            // Finally, we bind the incoming connection to our `hello` service
            if let Err(err) = http1::Builder::new()
                // `service_fn` converts our function in a `Service`
                .serve_connection(io, service_fn(serve))
                .await
            {
                eprintln!("Error serving connection: {:?}", err);
            }
        });
    }
    /*
    let server = Server::bind(&addr).serve(make_svc);

    if let Err(e) = server.await {
        eprintln!("server error: {}", e);
    }
    Ok(())
    */
}
