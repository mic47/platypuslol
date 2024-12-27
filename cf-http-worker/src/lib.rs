use std::{collections::HashMap, sync::Arc};

// TODO: maybe we do not need this dependency because of single enum
use http::StatusCode;
use worker::{event, Context, Env, Headers, HttpRequest, Response, ResponseBody, Result};

use redirect::CommonAppState;
use web_redirector::{query_params, route_get, ContentType, ServerResponse};

#[event(fetch)]
async fn fetch(req: HttpRequest, _env: Env, _ctx: Context) -> Result<Response> {
    console_error_panic_hook::set_once();
    let params: HashMap<String, String> = query_params(&req);
    let method = req.method().clone();
    let (head, body) = req.into_parts();
    let path = head.uri.path();
    let response = match method {
        http::Method::GET => {
            route_get(
                Arc::new(CommonAppState {
                    fallback: todo!(),
                    behavior: todo!(),
                    parser: todo!(),
                    loaded_config: todo!(),
                }),
                path,
                params,
                Arc::new(None), // TODO
                head.uri.host().unwrap_or_default().into(),
            )
            .map(|x| {
                match x {
                    ServerResponse::NotFound => Ok(not_found()),
                    ServerResponse::Json(value) => to_string_response(value, ContentType::Json),
                    ServerResponse::SuggestionsJson(value) => {
                        to_string_response(value, ContentType::SuggestionsJson)
                    }
                    ServerResponse::Html(value) => to_string_response(value, ContentType::Html),
                    ServerResponse::OpenSearchDescription(value) => {
                        to_string_response(value, ContentType::OpenSearchDescription)
                    }
                    ServerResponse::Bytes { data, content_type } => {
                        bytes_response(data, content_type)
                    }
                    ServerResponse::RedirectResponse { target } => redirect_response(target),
                }
                .unwrap_or_else(internal_server_error)
            })
        }
        _ => Ok(not_found()),
    }
    .unwrap_or_else(internal_server_error);
    Ok(response)
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
