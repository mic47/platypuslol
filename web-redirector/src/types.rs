pub type LastParsingError = std::sync::Arc<Option<anyhow::Error>>;

pub enum ContentType {
    Json,
    SuggestionsJson,
    Html,
    OpenSearchDescription,
    Png,
}

pub enum ServerResponse {
    NotFound,
    Json(serde_json::Value),
    SuggestionsJson(serde_json::Value),
    Html(String),
    OpenSearchDescription(String),
    Bytes {
        data: Vec<u8>,
        content_type: ContentType,
    },
    RedirectResponse {
        target: String,
    },
}
