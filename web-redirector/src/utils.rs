use std::collections::HashMap;

pub fn query_params<T>(req: &http::Request<T>) -> HashMap<String, String> {
    req.uri()
        .query()
        .map(|v| {
            url::form_urlencoded::parse(v.as_bytes())
                .into_owned()
                .collect()
        })
        .unwrap_or_default()
}
