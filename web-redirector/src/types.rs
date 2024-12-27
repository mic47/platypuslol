pub type LastParsingError = std::sync::Arc<Option<anyhow::Error>>;

pub enum ContentType {
    Json,
    SuggestionsJson,
    Html,
    OpenSearchDescription,
    Png,
}

impl ContentType {
    pub fn to_str(&self) -> &'static str {
        match self {
            ContentType::Json => "application/json",
            ContentType::SuggestionsJson => "application/x-suggestions+json",
            ContentType::Html => "text/html",
            ContentType::OpenSearchDescription => "application/opensearchdescription+xml",
            ContentType::Png => "image/png",
        }
    }
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
