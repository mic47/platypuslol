use std::{collections::HashMap, sync::Arc};

// TODO: maybe we do not need this dependency because of single enum
use http::StatusCode;
use worker::{event, Context, Env, Headers, HttpRequest, Response, ResponseBody, Result};

use redirect::CommonAppState;
use web_redirector::{load_config, query_params, route_get, ContentType, ServerResponse};

lazy_static::lazy_static! {
    static ref APP_STATE: anyhow::Result<Arc<CommonAppState>> = load_config(std::path::Path::new("main.json"), |_path| {
        Ok(String::from("
{
  \"fallback\": {
    \"link\": \"https://www.google.com/search?q={query}\",
    \"query_prefix\": \"search google\",
    \"redirect_automatically\": false
  },
  \"external_configurations\": {
    \"builtin://commands/google.json\": {
      \"enabled\": true
    },
    \"builtin://commands/python.json\": {
      \"enabled\": true
    },
    \"builtin://commands/mic.json\": {
      \"enabled\": true
    },
    \"builtin://commands/github.json\": {
      \"enabled\": true
    }
  },
  \"substitutions\": {},
  \"redirects\": []
}
        "))
    });
}

#[event(fetch)]
async fn fetch(req: HttpRequest, _env: Env, _ctx: Context) -> Result<Response> {
    console_error_panic_hook::set_once();
    APP_STATE
        .as_ref()
        .map(|state| {
            let params: HashMap<String, String> = query_params(&req);
            let method = req.method().clone();
            let (head, _body) = req.into_parts();
            let path = head.uri.path().trim_end_matches('/');
            let response = match method {
                http::Method::GET => {
                    route_get(
                        state.clone(),
                        path,
                        params,
                        Arc::new(None), // TODO fill this
                        format!(
                            "{}://{}",
                            head.uri.scheme_str().unwrap_or("http"),
                            head.uri.host().unwrap_or_default(),
                        ),
                    )
                    .map(|x| {
                        match x {
                            ServerResponse::NotFound => Ok(not_found()),
                            ServerResponse::Json(value) => {
                                to_string_response(value, ContentType::Json)
                            }
                            ServerResponse::SuggestionsJson(value) => {
                                to_string_response(value, ContentType::SuggestionsJson)
                            }
                            ServerResponse::Html(value) => {
                                to_string_response(value, ContentType::Html)
                            }
                            ServerResponse::OpenSearchDescription(value) => {
                                to_string_response(value, ContentType::OpenSearchDescription)
                            }
                            ServerResponse::Bytes { data, content_type } => {
                                bytes_response(data, content_type)
                            }
                            ServerResponse::RedirectResponse { target } => {
                                redirect_response(target)
                            }
                        }
                        .unwrap_or_else(internal_server_error)
                    })
                }
                _ => Ok(not_found()),
            }
            .unwrap_or_else(internal_server_error);
            Ok(response)
        })
        .unwrap_or_else(|x| Ok(internal_server_error(x)))
}

fn redirect_response(target: String) -> Result<Response> {
    let mut headers = Headers::new();
    headers.append("location", &target)?;
    Ok(Response::builder()
        .with_headers(headers)
        .with_status(StatusCode::FOUND.into())
        .body(ResponseBody::Empty))
}

fn ct_headers(content_type: ContentType) -> Result<Headers> {
    let mut headers = Headers::new();
    headers.append("content-type", content_type.to_str())?;
    Ok(headers)
}

fn bytes_response(value: Vec<u8>, content_type: ContentType) -> Result<Response> {
    Ok(Response::builder()
        .with_status(StatusCode::OK.into())
        .with_headers(ct_headers(content_type)?)
        .body(ResponseBody::Body(value)))
}
fn to_string_response<T: ToString>(value: T, content_type: ContentType) -> Result<Response> {
    Ok(Response::builder()
        .with_status(StatusCode::OK.into())
        .with_headers(ct_headers(content_type)?)
        .body(ResponseBody::Body(value.to_string().into())))
}

fn not_found() -> Response {
    Response::builder()
        .with_status(StatusCode::NOT_FOUND.into())
        .body(ResponseBody::Empty)
}

fn internal_server_error<T: ToString>(error: T) -> Response {
    Response::builder()
        .with_status(StatusCode::INTERNAL_SERVER_ERROR.into())
        .body(ResponseBody::Body(error.to_string().into()))
}
