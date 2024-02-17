use std::collections::{HashMap, HashSet};
use std::convert::Infallible;
use std::fmt::Write;
use std::net::SocketAddr;
use std::path::{Path, PathBuf};
use std::sync::{Arc, RwLock};

use anyhow::Context;
use html_builder::{Buffer, Html5, Node};
use hyper::body::Buf;
use hyper::http::HeaderValue;
use hyper::service::{make_service_fn, service_fn};
use hyper::{Body, Client, Method, Request, Response, Server, StatusCode, Uri};
use itertools::Itertools;
use notify::{Event, INotifyWatcher, RecursiveMode, Watcher};

use redirect::{
    resolve_parsed_output, resolve_suggestion_output, CommonAppState, Config, ConfigUrl,
    ExternalParser, QueryToken, RedirectConfig, ResolvedOutputMetadata,
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
const LIST_CSS: &str = include_str!("../../resources/list.css");

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
            .map(|x| resolve_parsed_output(x, &None).0)
            .collect();
        let suggested: Vec<_> = suggested
            .into_iter()
            .map(|x| resolve_suggestion_output(x, &None).0)
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
        for p in parsed
            .into_iter()
            .map(|x| resolve_parsed_output(x, &None).0)
        {
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
            .map(|x| resolve_suggestion_output(x, &None).0)
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

fn redirect(
    req: Request<Body>,
    state: Arc<CommonAppState>,
    last_parsing_error: LastParsingError,
) -> anyhow::Result<Response<Body>> {
    let p = query_params(&req);
    if let Some(query) = p.get("q") {
        if let Some(link) = state.redirect(query) {
            return redirect_response(&link);
        }
        return list(req, state, last_parsing_error, Some(query));
    }
    list(req, state, last_parsing_error, None)
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

fn add_key_class(class: Option<String>, parent: String, item: Node) -> Node {
    if let Some(class) = class {
        item.attr(&format!(
            "class='toogable onpress{} onparent{}'",
            class, parent
        ))
    } else {
        item
    }
}

#[derive(Debug)]
enum NestedList<I, T> {
    Element(T),
    Items(I, Vec<NestedList<I, T>>),
}

const FAST_SHORTCUT_CHARACTERS: &str = "fjdksla"; //"hgrueiwoqptyvncmxbz1234567890";

fn character_iterator(
    total: usize,
    css_prefix: String,
) -> Box<dyn Iterator<Item = (String, String)>> {
    let mut out = FAST_SHORTCUT_CHARACTERS
        .chars()
        .map(|c| c.to_string())
        .collect::<Vec<_>>();
    while out.len() < total {
        out = out
            .into_iter()
            .flat_map(|s| {
                FAST_SHORTCUT_CHARACTERS
                    .chars()
                    .map(|c| format!("{}{}", s, c))
                    .collect::<Vec<_>>()
            })
            .collect();
    }

    Box::new(
        out.into_iter()
            .map(move |c| (format!("[{}] ", c), format!("{}{}", css_prefix, c))),
    )
}

#[allow(clippy::type_complexity)]
fn list_nest<I: Iterator<Item = (String, String)>>(
    available_key_classes: &mut I,
    css_prefix: String,
    list: &mut Node,
    elements: NestedState,
) -> anyhow::Result<()> {
    match elements {
        NestedList::Element(NestedStateItem {
            description,
            links,
            meta: _meta,
            suffix_text,
            key,
        }) => {
            if let Some(links) = links {
                if let [link] = &links[..] {
                    let maybe_key = key.or_else(|| available_key_classes.next());
                    writeln!(
                        // TODO: escape link?
                        add_key_class(
                            maybe_key.clone().map(|x| x.1),
                            css_prefix,
                            list.li().a().attr(&format!("href='{}'", link))
                        ),
                        "{}🔗 {}{}",
                        maybe_key.map(|x| x.0).unwrap_or_default(),
                        description,
                        suffix_text,
                    )?;
                } else {
                    let mut li = list.li();
                    let maybe_key = key.or_else(|| available_key_classes.next());
                    let mut div = add_key_class(
                        maybe_key.clone().map(|x| x.1),
                        css_prefix.clone(),
                        li.span(),
                    );
                    let mut span = div.span().attr("class='aslink'").attr(&format!(
                        "onclick='redirect(\"{}\")'",
                        maybe_key.clone().map(|x| x.1).unwrap_or_default(),
                    ));
                    writeln!(
                        span,
                        "{}🔗 {}{}",
                        maybe_key.clone().map(|x| x.0).unwrap_or_default(),
                        description,
                        suffix_text,
                    )?;
                    let mut ul = div.ul();
                    for link in links {
                        writeln!(
                            // TODO: escape link?
                            add_key_class(
                                maybe_key.clone().map(|x| x.1),
                                css_prefix.clone(),
                                ul.li().a().attr(&format!("href='{}'", link))
                            ),
                            "{}",
                            link,
                        )?;
                    }
                }
            } else {
                writeln!(list.li(), "{}", description,)?;
            }
        }
        NestedList::Items(group, items) => {
            let mut li = list.li();
            let maybe_key = available_key_classes.next();
            let mut available_key_classes =
                character_iterator(items.len(), maybe_key.clone().unwrap_or_default().1);
            let mut hideable_span = add_key_class(
                maybe_key.clone().map(|x| x.1),
                css_prefix.clone(),
                li.span(),
            );
            let mut span = hideable_span.span().attr("class='aslink'").attr(&format!(
                "onclick='setQuery(\"{}\")'",
                maybe_key.clone().map(|x| x.1).unwrap_or_default()
            ));
            writeln!(
                span,
                "{}📂 {}",
                maybe_key.clone().map(|x| x.0).unwrap_or_default(),
                group
                    .into_iter()
                    .map(|x| match x {
                        redirect::QueryToken::Exact(x) => x,
                        redirect::QueryToken::Prefix(x) => x,
                        redirect::QueryToken::Regex(x, _) => x,
                        redirect::QueryToken::Substitution(x, _, _) => format!("<{x}>"),
                    })
                    .join(" ")
            )?;
            let mut ul = li.ul();
            for item in items.into_iter() {
                list_nest(
                    &mut available_key_classes,
                    maybe_key.clone().map(|x| x.1).unwrap_or_default(),
                    &mut ul,
                    item,
                )?;
            }
        }
    };
    Ok(())
}

fn list(
    req: Request<Body>,
    state: Arc<CommonAppState>,
    last_parsing_error: LastParsingError,
    failed_query: Option<&str>,
) -> anyhow::Result<Response<Body>> {
    let mut failed_matches = vec![];
    if let Some(failed_query) = failed_query {
        let (parsed, _) = state
            .fallback
            .parser
            .parse_full_and_suggest(&state.fallback.behavior.make_query(failed_query));
        if let Some((p, meta)) = parsed
            .into_iter()
            .map(|x| resolve_parsed_output(x, &None))
            .next()
        {
            failed_matches.push(NestedStateItem {
                description: p.description,
                links: Some(p.links),
                meta,
                key: Some(("[u] ".to_string(), "u".to_string())),
                suffix_text: " <- defa[u]lt",
            });
        }
    }

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
    for (p, meta) in parsed
        .into_iter()
        .map(|x| resolve_parsed_output(x, &failed_query.map(Into::into)))
    {
        matches.push(NestedStateItem {
            description: p.description,
            links: Some(p.links),
            meta,
            key: None,
            suffix_text: "",
        })
    }
    let mut visited: HashSet<_> = HashSet::default();
    for (s, meta) in suggested
        .into_iter()
        .map(|x| resolve_suggestion_output(x, &failed_query.map(Into::into)))
    {
        if !visited.insert(s.clone()) {
            continue;
        }
        matches.push(NestedStateItem {
            description: s.description,
            links: s.links,
            meta,
            key: None,
            suffix_text: "",
        })
    }
    let first = matches.first().cloned().map(|item| NestedStateItem {
        key: Some(("[i] ".to_string(), "i".to_string())),
        suffix_text: " <- F[i]rst match",
        ..item
    });
    matches.sort_by_key(|x| {
        (
            x.meta.command.clone(),
            x.description.clone(),
            x.links.clone(),
        )
    });

    let grouped = first
        .into_iter()
        .chain(failed_matches.into_iter().chain(matches))
        .group_by(|x| x.meta.command.clone());
    let grouped = grouped.into_iter().collect::<Vec<_>>();

    let mut groups = vec![];
    for group in grouped.into_iter() {
        let group_list = group.1.collect::<Vec<_>>();
        if group_list.len() == 1 {
            let mut group_list = group_list;
            groups.push(NestedList::Element(group_list.pop().unwrap()));
        } else {
            groups.push(NestedList::Items(
                group.0,
                group_list.into_iter().map(NestedList::Element).collect(),
            ));
        }
    }

    let groups = simplify(split_by_tokens(
        groups,
        FAST_SHORTCUT_CHARACTERS.len() * FAST_SHORTCUT_CHARACTERS.len(),
    ));
    let mut available_key_classes = character_iterator(groups.len(), "".into());

    list_page_head(used_query, move |body| {
        let mut list = body.ul();
        for group in groups.into_iter() {
            list_nest(&mut available_key_classes, "".into(), &mut list, group)?;
        }
        if let Some(error) = last_parsing_error.as_ref() {
            writeln!(
                body.h2(),
                "Unable to load config, using old one. Last error:"
            )?;
            writeln!(body.pre(), "{:?}", error,)?;
        };
        Ok(())
    })
}

fn list_page_head<F: FnOnce(&mut Node) -> anyhow::Result<()>>(
    used_query: Option<&String>,
    content_function: F,
) -> anyhow::Result<Response<Body>> {
    let mut buf = Buffer::new();
    let mut html = buf.html().attr("lang='en'");
    let _meta = html.meta().attr("charset=\"UTF-8\"");
    let mut head = html.head();
    writeln!(head.title(), "List of platypus lol commands")?;
    writeln!(head.script().raw(), "{}", LIST_JS)?;
    writeln!(head.style().raw(), "{}", LIST_CSS)?;
    let mut body_impl = html.body().attr("onload='onLoad()'");
    let mut body = body_impl.div().attr("class='centered'");
    if let Some(used_query) = used_query {
        writeln!(body.h1(), "List of Commands for Query '{}'", used_query)?;
    } else {
        writeln!(body.h1(), "List of All Commands")?;
    }
    writeln!(body, "ℹ️ Type e to expand all folders. Type text in [brackets] to visit a link🔗 or expand a folder📂. [i] is always first match and [u] is default query.")?;
    body.br();
    let mut div = body.div();
    writeln!(div, "Typed text: ")?;
    div.input().attr("id='query'").attr("disabled");
    div.input()
        .attr("id='expand'")
        .attr("type='checkbox'")
        .attr("onchange='redraw()'");
    writeln!(div, "[e]xpand all")?;
    writeln!(div.button().attr("onclick='reset()'"), "reset [esc]")?;
    content_function(&mut body)?;
    to_string_response(buf.finish(), ContentType::Html)
}

type NestedState<'a> = NestedList<Vec<QueryToken>, NestedStateItem<'a>>;

#[derive(Clone, Debug)]
struct NestedStateItem<'a> {
    description: String,
    links: Option<Vec<String>>,
    meta: ResolvedOutputMetadata,
    suffix_text: &'a str,
    key: Option<(String, String)>,
}

fn split_by_tokens(list: Vec<NestedState>, max_size: usize) -> Vec<NestedState> {
    let input_len = list.len();
    let list = list
        .into_iter()
        .map(|x| match x {
            NestedList::Element(_) => x,
            NestedList::Items(x, items) => NestedList::Items(x, split_by_tokens(items, max_size)),
        })
        .collect::<Vec<_>>();
    if list.len() > max_size {
        let mut counts: HashMap<usize, HashSet<u64>> = HashMap::new();
        let max_width = list
            .iter()
            .map(|x| match x {
                NestedList::Element(item) => item.meta.command.len(),
                NestedList::Items(query, _) => query.len(),
            })
            .max()
            .unwrap_or_default();
        let empty_query = Default::default();
        let empty_subst = Default::default();
        for item in list.iter() {
            let (command, query, substitutions) = match item {
                NestedList::Element(item) => (
                    &item.meta.command,
                    &item.meta.query,
                    &item.meta.substitutions,
                ),
                NestedList::Items(command, _) => (command, &empty_query, &empty_subst),
            };
            let hashes = QueryToken::content_hashes(command, query, substitutions, &None);
            if let Some(last) = hashes.last() {
                for i in hashes.len()..max_width {
                    counts.entry(i).or_default().insert(*last);
                }
            }
            for (i, hsh) in hashes.into_iter().enumerate() {
                counts.entry(i).or_default().insert(hsh);
            }
        }
        let (small, large): (Vec<_>, Vec<_>) = counts
            .into_iter()
            .map(|(k, v)| (k, v.len()))
            .partition(|(_, v)| v <= &max_size);
        let to_take = small
            .into_iter()
            .map(|(k, _)| k)
            .max()
            .unwrap_or_else(|| large.into_iter().map(|(k, _)| k).min().unwrap_or(max_width));
        let mut out = vec![];
        for (tokens, group) in list
            .into_iter()
            .group_by(|x| match x {
                NestedList::Element(item) => item
                    .meta
                    .command
                    .iter()
                    .map(|token| {
                        QueryToken::Exact(token.to_description(
                            &item.meta.query,
                            &item.meta.substitutions,
                            &None,
                            true,
                        ))
                    })
                    .take(to_take + 1)
                    .collect::<Vec<_>>(),
                NestedList::Items(item, _) => {
                    item.iter().take(to_take + 1).cloned().collect::<Vec<_>>()
                }
            })
            .into_iter()
        {
            let mut items = group.collect::<Vec<_>>();
            if items.len() == 1 {
                if let Some(x) = items.pop() {
                    out.push(x)
                }
            } else if items.len() != input_len {
                out.push(NestedList::Items(tokens, split_by_tokens(items, max_size)));
            } else {
                // To prevent infinite recursion, don't split if grouping didn't decrease
                // size of the list.
                out.push(NestedList::Items(tokens, items));
            }
        }
        out
    } else {
        list
    }
}

#[allow(clippy::type_complexity)]
fn simplify(list: Vec<NestedState>) -> Vec<NestedState> {
    let mut out = vec![];
    let mut prev_items: Option<(
        Vec<String>,
        Vec<QueryToken>,
        Vec<NestedList<Vec<QueryToken>, NestedStateItem>>,
    )> = None;
    for item in list.into_iter() {
        match item {
            NestedList::Element(_) => {
                if let Some((_, prev_x, prev_list)) = prev_items {
                    let mut prev_list = simplify(prev_list);
                    if prev_list.len() == 1 {
                        out.push(prev_list.pop().unwrap())
                    } else {
                        out.push(NestedList::Items(prev_x, prev_list))
                    }
                }
                out.push(item);
                prev_items = None;
            }
            NestedList::Items(x, items) => {
                let description = x
                    .iter()
                    .map(|t| {
                        t.to_description(&HashMap::default(), &HashMap::default(), &None, true)
                    })
                    .collect::<Vec<_>>();
                if let Some((prev_description, prev_x, mut prev_list)) = prev_items {
                    if prev_description == description {
                        prev_list.extend(items);
                        prev_items = Some((prev_description, prev_x, prev_list));
                    } else {
                        let mut prev_list = simplify(prev_list);
                        if prev_list.len() == 1 {
                            out.push(prev_list.pop().unwrap())
                        } else {
                            out.push(NestedList::Items(prev_x, prev_list))
                        }
                        prev_items = Some((description, x, items));
                    }
                } else {
                    prev_items = Some((description, x, items));
                }
            }
        }
    }
    if let Some((_, prev_x, prev_list)) = prev_items {
        let mut prev_list = simplify(prev_list);
        if prev_list.len() == 1 {
            out.push(prev_list.pop().unwrap())
        } else {
            out.push(NestedList::Items(prev_x, simplify(prev_list)))
        }
    }
    out
}

fn route(
    req: Request<Body>,
    state: Arc<RwLock<Arc<CommonAppState>>>,
    last_parsing_error: Arc<RwLock<LastParsingError>>,
    template_variables: HashMap<String, String>,
) -> Result<Response<Body>, Infallible> {
    let method = req.method();
    let path = req.uri().path();
    let path = path.strip_suffix('/').unwrap_or(path);
    let state: Arc<CommonAppState> = { state.read().unwrap().clone() };
    let last_parsing_error: LastParsingError = { last_parsing_error.read().unwrap().clone() };
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
        (&Method::GET, "/list") => list(req, state, last_parsing_error, None),
        (&Method::GET, "/debug") => debug(req, state),
        (&Method::GET, "/redirect") => redirect(req, state, last_parsing_error),
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

type LastParsingError = Arc<Option<anyhow::Error>>;

fn config_watcher(
    path: PathBuf,
    state: Arc<RwLock<Arc<CommonAppState>>>,
    last_parsing_error: Arc<RwLock<LastParsingError>>,
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

    let parser = Arc::new(RwLock::new(load_config(&cli.link_config).await?));
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
                let template_variables = HashMap::from([("{server}".into(), server_uri)]);
                async { route(req, parser, last_parsing_error, template_variables) }
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
