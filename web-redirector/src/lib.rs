use std::collections::{HashMap, HashSet};
use std::fmt::Write;
use std::sync::Arc;

use anyhow::Context;
use html_builder::{Buffer, Html5, Node};
use itertools::Itertools;

use nfa::{Parsed, Suggestion};
use redirect::{
    resolve_parsed_output, resolve_suggestion_output, CommonAppState, Config, ConfigLinkQuery,
    ConfigLinkQueryFile, LinkToken, QueryToken, RedirectConfig, ResolvedOutputMetadata,
    ResolvedSuggestionOutput,
};

const INSTALL_INSTRUCTIONS: &str = include_str!("../../resources/index.html");
const OPENSEARCH: &str = include_str!("../../resources/opensearch.xml");
lazy_static::lazy_static! {
    static ref ICONS: HashMap<&'static str, Vec<u8>> = HashMap::from([
        ("/favicon-16x16.png", Vec::from(*include_bytes!("../../resources/favicon-16x16.png"))),
        ("/favicon-32x32.png", Vec::from(*include_bytes!("../../resources/favicon-32x32.png"))),
        ("/favicon-96x96.png", Vec::from(*include_bytes!("../../resources/favicon-96x96.png"))),
        ("/favicon.ico", Vec::from(*include_bytes!("../../resources/favicon.ico"))),
    ]);
}

const LIST_JS: &str = include_str!("../../resources/keypress.js");
const LIST_CSS: &str = include_str!("../../resources/list.css");

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

fn templated_string(value: String, template_variables: &HashMap<String, String>) -> String {
    let mut value = value;
    for (k, v) in template_variables {
        value = value.replace(k, v);
    }
    value
}

pub type LastParsingError = Arc<Option<anyhow::Error>>;

pub fn route_get(
    state: Arc<CommonAppState>,
    path: &str,
    request_params: HashMap<String, String>,
    last_parsing_error: LastParsingError,
    template_variables: HashMap<String, String>,
) -> anyhow::Result<ServerResponse> {
    match path {
        "" | "/install" => Ok(ServerResponse::Html(templated_string(
            INSTALL_INSTRUCTIONS.into(),
            &template_variables,
        ))),
        "/install/opensearch.xml" => Ok(ServerResponse::OpenSearchDescription(templated_string(
            OPENSEARCH.into(),
            &template_variables,
            //ContentType::OpenSearchDescription,
        ))),
        "/list" => list(&request_params, state, last_parsing_error, None),
        "/config" => config(&request_params, state, last_parsing_error),
        "/debug" => debug(&request_params, state),
        "/redirect" => redirect(&request_params, state, last_parsing_error),
        "/suggest" => suggest(&request_params, state),
        path => {
            if let Some(icon) = ICONS.get(&path) {
                Ok(ServerResponse::Bytes {
                    data: icon.clone(),
                    content_type: ContentType::Png,
                })
            } else {
                Ok(ServerResponse::NotFound)
            }
        }
    }
}

fn debug(
    request_params: &HashMap<String, String>,
    state: Arc<CommonAppState>,
) -> anyhow::Result<ServerResponse> {
    if let Some(query) = request_params.get("q") {
        let (parsed, suggested) = state.parser.parse_full_and_suggest(query);
        let parsed: Vec<_> = parsed
            .into_iter()
            .map(|x| resolve_parsed_output(x, &None).0)
            .collect();
        let suggested: Vec<_> = suggested
            .into_iter()
            .map(|x| resolve_suggestion_output(x, &None).0)
            .collect();
        Ok(ServerResponse::Json(
            serde_json::to_value((parsed, suggested)).context("Unable to serialize to json")?,
        ))
    } else {
        Ok(ServerResponse::NotFound)
    }
}

fn suggest(
    query_params: &HashMap<String, String>,
    state: Arc<CommonAppState>,
) -> anyhow::Result<ServerResponse> {
    let mut suggestions_left: i32 = 20; // TODO: configure
    if let Some(query) = query_params.get("q") {
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

        Ok(ServerResponse::Json(
            serde_json::to_value((query, suggested_queries, suggested_texts, suggested_urls))
                .context("Unable to serialize suggestions")?,
        ))
    } else {
        Ok(ServerResponse::NotFound)
    }
}

fn list(
    query_params: &HashMap<String, String>,
    state: Arc<CommonAppState>,
    last_parsing_error: LastParsingError,
    failed_query: Option<&str>,
) -> anyhow::Result<ServerResponse> {
    let (used_query, groups) = list_get_groups(
        query_params.get("q").map(|x| x.as_str()),
        state,
        failed_query,
    );
    let (width, mut available_key_classes) = character_iterator(groups.len(), "".into());

    list_page_head(used_query, last_parsing_error, move |body| {
        let mut list = body.ul();
        for group in groups.into_iter() {
            list_nest(
                &mut available_key_classes,
                width,
                "".into(),
                &mut list,
                group,
                /* nesting */ 0,
            )?;
        }
        Ok(())
    })
}

fn list_interface(body: &mut Node) -> anyhow::Result<()> {
    let mut div = body.div();
    writeln!(div, "Typed text: ")?;
    div.input().attr("id='query'").attr("disabled");
    div.input()
        .attr("id='expand'")
        .attr("type='checkbox'")
        .attr("onchange='redraw()'");
    writeln!(div, "[e]xpand all")?;
    writeln!(div.button().attr("onclick='reset()'"), "reset [esc]")?;
    Ok(())
}

fn redirect_response(uris: &[String]) -> anyhow::Result<ServerResponse> {
    if let [uri] = uris {
        Ok(ServerResponse::RedirectResponse { target: uri.into() })
    } else {
        redirect_multi(uris).map(ServerResponse::Html)
    }
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

fn redirect(
    request_params: &HashMap<String, String>,
    state: Arc<CommonAppState>,
    last_parsing_error: LastParsingError,
) -> anyhow::Result<ServerResponse> {
    if let Some(query) = request_params.get("q") {
        if let Some(link) = state.redirect(query) {
            return redirect_response(&link);
        }
        return list(request_params, state, last_parsing_error, Some(query));
    }
    list(request_params, state, last_parsing_error, None)
}

fn error_in_config(body: &mut Node, last_parsing_error: LastParsingError) -> anyhow::Result<()> {
    if let Some(error) = last_parsing_error.as_ref() {
        let mut section = body.div().attr("class='error'");
        writeln!(
            section.h2(),
            "Unable to load config, using old one. Last error:"
        )?;
        writeln!(section.pre(), "{:?}", error)?;
    };
    Ok(())
}

fn list_page_head<F: FnOnce(&mut Node) -> anyhow::Result<()>>(
    used_query: Option<String>,
    last_parsing_error: LastParsingError,
    content_function: F,
) -> anyhow::Result<ServerResponse> {
    let mut buf = Buffer::new();
    let mut html = buf.html().attr("lang='en'");
    let _meta = html.meta().attr("charset=\"UTF-8\"");
    let mut head = html.head();
    writeln!(head.title(), "List of platypus lol commands")?;
    writeln!(head.script().raw(), "{}", LIST_JS)?;
    writeln!(head.style().raw(), "{}", LIST_CSS)?;
    let mut body_impl = html
        .body()
        .attr("onload='onLoad()'")
        .attr("class='list_commands'");
    let mut body = body_impl.div().attr("class='centered'");
    error_in_config(&mut body, last_parsing_error)?;
    if let Some(used_query) = used_query {
        writeln!(body.h1(), "List of Commands for Query '{}'", used_query)?;
    } else {
        writeln!(body.h1(), "List of All Commands")?;
    }
    writeln!(body, "ℹ️ Type e to expand all folders. Type text in [brackets] to visit a link🔗 or expand a folder📂. [i] is always first match and [u] is default query.")?;
    body.br();
    list_interface(&mut body)?;
    content_function(&mut body)?;
    Ok(ServerResponse::Html(buf.finish()))
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
            .chunk_by(|x| match x {
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

fn add_key_class(class: Option<String>, parent: String, item: Node, nesting: usize) -> Node {
    if let Some(class) = class {
        let attr = format!("class='toogable onpress{} onparent{}'", class, parent);
        if nesting > 0 {
            item.attr(&attr).attr("style='display:none;'")
        } else {
            item.attr(&attr)
        }
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
) -> (usize, Box<dyn Iterator<Item = (String, String)>>) {
    let mut out = FAST_SHORTCUT_CHARACTERS
        .chars()
        .map(|c| c.to_string())
        .collect::<Vec<_>>();
    let mut width = 1;
    while out.len() < total {
        width += 1;
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

    (
        width,
        Box::new(
            out.into_iter()
                .map(move |c| (format!("[{}] ", c), format!("{}{}", css_prefix, c))),
        ),
    )
}

#[allow(clippy::type_complexity)]
fn list_nest<I: Iterator<Item = (String, String)>>(
    available_key_classes: &mut I,
    width: usize,
    css_prefix: String,
    list: &mut Node,
    elements: NestedState,
    nesting: usize,
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
                if links.is_empty() {
                    writeln!(
                        add_key_class(
                            Some(format!("{}__", css_prefix)),
                            css_prefix,
                            list.li(),
                            nesting
                        )
                        .span(),
                        "[{}] ⛔ {}{}",
                        String::from_utf8(vec![b'_'; width])?,
                        description,
                        suffix_text,
                    )?;
                } else if let [link] = &links[..] {
                    let maybe_key = key.or_else(|| available_key_classes.next());
                    writeln!(
                        // TODO: escape link?
                        add_key_class(
                            maybe_key.clone().map(|x| x.1),
                            css_prefix,
                            list.li(),
                            nesting
                        )
                        .a()
                        .attr(&format!("href='{}'", link)),
                        "{}🔗 {}{}",
                        maybe_key.map(|x| x.0).unwrap_or_default(),
                        description,
                        suffix_text,
                    )?;
                } else {
                    let maybe_key = key.or_else(|| available_key_classes.next());
                    let mut li = add_key_class(
                        maybe_key.clone().map(|x| x.1),
                        css_prefix.clone(),
                        list.li(),
                        nesting,
                    );
                    let mut div = li.span();
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
                                ul.li(),
                                nesting,
                            )
                            .a()
                            .attr(&format!("href='{}'", link)),
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
            let maybe_key = available_key_classes.next();
            let (width, mut available_key_classes) =
                character_iterator(items.len(), maybe_key.clone().unwrap_or_default().1);
            let mut li = add_key_class(
                maybe_key.clone().map(|x| x.1),
                css_prefix.clone(),
                list.li(),
                nesting,
            );
            let mut hideable_span = li.span();
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
                        redirect::QueryToken::Prefix {
                            word,
                            min_length: _,
                        } => word,
                        redirect::QueryToken::Regex(x, _) => x,
                        redirect::QueryToken::Substitution {
                            name,
                            type_: _,
                            subtype: _,
                        } => format!("<{name}>"),
                    })
                    .join(" ")
            )?;
            let mut ul = li.ul();
            for item in items.into_iter() {
                list_nest(
                    &mut available_key_classes,
                    width,
                    maybe_key.clone().map(|x| x.1).unwrap_or_default(),
                    &mut ul,
                    item,
                    nesting + 1,
                )?;
            }
        }
    };
    Ok(())
}

fn list_get_groups<'a>(
    param_q: Option<&str>,
    state: Arc<CommonAppState>,
    failed_query: Option<&str>,
) -> (
    Option<String>,
    Vec<NestedList<Vec<QueryToken>, NestedStateItem<'a>>>,
) {
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

    let (used_query, parsed_and_suggested) = param_q
        .map(|q| {
            let (p, s) = state.parser.parse_full_and_suggest(q);
            if p.is_empty() && s.is_empty() {
                (None, None)
            } else {
                (Some(q), Some((p, s)))
            }
        })
        .unwrap_or((None, None));
    let mut visited: HashSet<_> = HashSet::default();
    let mut all_matches = vec![];
    let mut first = None;
    if let Some((parsed, suggested)) = parsed_and_suggested {
        let mut matches =
            get_matches_from_parse_and_suggest(parsed, suggested, &mut visited, failed_query);
        first = matches.first().cloned().map(|item| NestedStateItem {
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
        all_matches.extend(matches);
    } else {
        let q = param_q.unwrap_or("");
        for prefix_len in (0..q.len() + 1).rev() {
            if let Some((q, _)) = q.split_at_checked(prefix_len) {
                let (parsed, suggested) = state.parser.parse_full_and_suggest(q);
                let mut matches = get_matches_from_parse_and_suggest(
                    parsed,
                    suggested,
                    &mut visited,
                    failed_query,
                );
                matches.sort_by_key(|x| {
                    (
                        x.meta.command.clone(),
                        x.description.clone(),
                        x.links.clone(),
                    )
                });
                if !matches.is_empty() {
                    all_matches.push(NestedStateItem {
                        description: format!("Matches & suggestions for prefix query: \"{}\"", q),
                        links: None,
                        meta: ResolvedOutputMetadata {
                            command: vec![],
                            query: HashMap::new(),
                            substitutions: HashMap::new(),
                        },
                        suffix_text: "",
                        key: None,
                    });
                    all_matches.extend(matches);
                }
            }
        }
    };

    let grouped = first
        .into_iter()
        .chain(failed_matches.into_iter().chain(all_matches))
        .chunk_by(|x| x.meta.command.clone());
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

    (
        used_query.map(String::from),
        simplify(split_by_tokens(
            groups,
            FAST_SHORTCUT_CHARACTERS.len() * FAST_SHORTCUT_CHARACTERS.len(),
        )),
    )
}

#[allow(clippy::type_complexity)]
fn get_matches_from_parse_and_suggest<'a>(
    parsed: Vec<Parsed<(Vec<Vec<LinkToken>>, Vec<QueryToken>)>>,
    suggested: Vec<Suggestion<(Vec<Vec<LinkToken>>, Vec<QueryToken>)>>,
    visited: &mut HashSet<ResolvedSuggestionOutput>,
    failed_query: Option<&str>,
) -> Vec<NestedStateItem<'a>> {
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
        });
    }
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
        });
    }
    matches
}

fn config(
    request_params: &HashMap<String, String>,
    state: Arc<CommonAppState>,
    last_parsing_error: LastParsingError,
) -> anyhow::Result<ServerResponse> {
    let mut buf = Buffer::new();
    let mut html = buf.html().attr("lang='en'");
    let _meta = html.meta().attr("charset=\"UTF-8\"");
    let mut head = html.head();
    writeln!(head.script().raw(), "{}", LIST_JS)?;
    writeln!(head.style().raw(), "{}", LIST_CSS)?;
    let mut body_impl = html.body().attr("onload='onLoad()'");
    let mut body = body_impl.div().attr("class='centered_wide'");
    error_in_config(&mut body, last_parsing_error)?;

    {
        // Config tester section
        let mut section = body.div().attr("class='config_section'");
        writeln!(section.h2(), "Config query tester")?;
        if request_params.contains_key("test") {
            section.br();
            let (test_query, query_with_links, config) =
                config_parse_params_and_get_updated_config(request_params, state.as_ref());
            match CommonAppState::new(config) {
                Ok(state) => {
                    config_success_test(&mut section, state, &test_query, query_with_links)?;
                }
                Err(err) => {
                    let mut section = section.div().attr("class='error'");
                    writeln!(section, "There was an error parsing config:")?;
                    section.br();
                    writeln!(section.pre(), "{:?}", err)?;
                }
            }
        }
        config_form(&mut section, request_params)?;
    }

    let mut section = body.div().attr("class='config_section'");
    let mut config_ids = 0;
    config_entry(
        "Main config",
        &state.loaded_config.redirects,
        &mut section,
        &mut config_ids,
        &[],
    )?;

    for (cname, config) in state.loaded_config.external_configurations.iter() {
        if !config.enabled {
            continue;
        }
        if let Some(ref inner_config) = config.config {
            let mut section = body.div().attr("class='config_section'");
            config_entry(
                &format!("{:?}", cname),
                inner_config,
                &mut section,
                &mut config_ids,
                &config.substitutions_to_inherit,
            )?;
        }
    }
    Ok(ServerResponse::Html(buf.finish()))
}

fn config_entry(
    section_name: &str,
    config: &RedirectConfig<String>,
    section: &mut Node,
    config_ids: &mut usize,
    substitutions_to_inherit: &[String],
) -> anyhow::Result<()> {
    let substitutions_to_inherit = substitutions_to_inherit.iter().collect::<HashSet<_>>();
    writeln!(section.h2(), "{}", section_name)?;
    for (sname, subst) in config.substitutions.iter() {
        writeln!(section.h3(), "Substitution: '{}'", sname)?;
        if substitutions_to_inherit.contains(sname) {
            writeln!(
                section,
                "ℹ️ This substutution is usable in the main config."
            )?;
        }
        let mut keys = HashSet::new();
        subst.iter().for_each(|x| keys.extend(x.keys()));
        let mut keys = keys.into_iter().collect::<Vec<_>>();
        keys.sort();
        let mut table = section.table();
        let mut header = table.tr();
        writeln!(header.th(), "field")?;
        writeln!(header.th(), "Use in query")?;
        writeln!(header.th(), "Use in link")?;
        for key in keys.iter() {
            let mut row = table.tr();
            writeln!(row.th(), "{}", key)?;
            writeln!(row.td(), "{{:subst:{}:{}}}", sname, key)?;
            writeln!(row.td(), "{{{}:{}}}", sname, key)?;
        }

        let id = format!("cfgval_{}", config_ids);
        *config_ids += 1;
        writeln!(
            section
                .h4()
                .attr(&format!("onclick='flipVisibility(\"{}\")'", id)),
            "Click to see '{}' values",
            sname
        )?;
        let mut table = section
            .table()
            .attr(&format!("id='{}'", id))
            .attr("style='display:none'");
        let mut header = table.tr();
        for key in keys.iter() {
            writeln!(header.th(), "{}", key)?;
        }
        let empty = String::default();
        for s in subst.iter() {
            let mut row = table.tr();
            for key in keys.iter() {
                writeln!(row.td(), "{}", s.get(*key).unwrap_or(&empty))?;
            }
        }
    }
    writeln!(section.h3(), "Redirects")?;
    let mut redirects = section.table();
    let mut header = redirects.tr();
    writeln!(header.th(), "query")?;
    writeln!(header.th(), "exact")?;
    writeln!(header.th(), "links")?;
    for command in config.redirects.iter() {
        let mut row = redirects.tr();
        writeln!(row.td(), "{}", command.query)?;
        writeln!(row.td(), "{}", command.exact)?;
        writeln!(row.td(), "{}", command.links.iter().join(", "))?;
    }
    Ok(())
}

#[allow(clippy::type_complexity)]
fn config_parse_params_and_get_updated_config(
    params: &HashMap<String, String>,
    state: &CommonAppState,
) -> (
    String,
    Option<(String, Vec<String>)>,
    Config<String, RedirectConfig<String>>,
) {
    let test_query = params.get("test_query").cloned().unwrap_or_default();
    if let (Some(query), Some(links)) = (
        params.get("query").cloned(),
        params.get("links").map(|x| {
            x.split('\n')
                .map(|x| x.trim().to_string())
                .filter(|x| !x.is_empty())
                .collect::<Vec<_>>()
        }),
    ) {
        let mut config = state.loaded_config.clone();
        config.redirects.redirects.push(ConfigLinkQuery {
            query: query.clone(),
            links: links.clone(),
            exact: false,
        });
        (
            test_query,
            if !query.is_empty() && !links.is_empty() {
                Some((query, links))
            } else {
                None
            },
            config,
        )
    } else {
        (test_query, None, state.loaded_config.clone())
    }
}

fn config_form(section: &mut Node, params: &HashMap<String, String>) -> anyhow::Result<()> {
    // TODO: add html escape
    let mut form = section.form();
    writeln!(form, "Test query: ")?;
    let test_query = form.input().attr("name='test_query'");
    params
        .get("test_query")
        .map(|q| test_query.attr(&format!("value='{}'", q)));
    form.br();
    writeln!(form, "Query: ")?;
    let query = form.input().attr("name='query'");
    params
        .get("query")
        .map(|q| query.attr(&format!("value='{}'", q)));
    form.br();
    writeln!(form, "Links: ")?;
    let mut links = form
        .textarea()
        .attr("name='links'")
        .attr("rows=2")
        .attr("cols=80");
    params.get("links").map(|q| writeln!(links, "{}", q));
    form.br();
    writeln!(
        form.button().attr("type='submit'").attr("name='test'"),
        "Test"
    )?;
    form.br();
    writeln!(form, "You can test above existing or new query commands. If new command is valid, this tool will show you snippet that you can put into documentation.")?;
    let mut list = form.ul();
    writeln!(list.li(), "Test query -- what you type into browser.")?;
    writeln!(
        list.li(),
        "Query -- query part of the command -- what is matched agains what you type into browser."
    )?;
    writeln!(list.li(), "Links -- one link per line.")?;
    writeln!(form, "Inside query and links, you can use following substitution variables, all inside braces '{{}}':")?;
    let mut list = form.ul();
    writeln!(
        list.li(),
        "{{word:exact}} -- \"word\" will be matched exactly"
    )?;
    writeln!(
        list.li(),
        "{{name:prefix}} -- match any prefix of the word."
    )?;
    writeln!(
        list.li(),
        "{{name:prefix:min_length}} -- match any prefix of the word that has length at least `{{min_length}}`."
    )?;
    writeln!(
        list.li(),
        "{{name:word}} -- match single word. You can use matched word in the link using '{{name}}'. You can use '{{name:|list-of-transformations}}' to transform query in the final url. 'list-of-transformations' is '|' separated list of 'URLEncode', 'AsIs', 'Uppercase', or 'LowerCase'. Note that it has to start with '|' character."
    )?;
    writeln!(list.li(), "{{name:query}} -- match any string, including white spaces. You can use matched string in the link using '{{name}}'. You can use `list-of-transformations` with query too.")?;
    writeln!(list.li(), "{{name:subst:type:K1}} -- match key 'K1' string from substitution of 'type'. In case 'name' is empty, 'type' is used as 'name'. You can use '{{name:K2}}' in the link to substitute to key 'K2' from matched substitution. Substitution is matched if it's a substring. For example 'ol' matches 'rofl', but 'lo' does not.")?;

    Ok(())
}

fn config_success_test(
    section: &mut Node,
    state: CommonAppState,
    test_query: &str,
    query_with_links: Option<(String, Vec<String>)>,
) -> anyhow::Result<()> {
    writeln!(section, "Config successfully tested. Following would be matched if you used '{}'. Click inside green area to test it interactively", test_query)?;
    section.br();
    let mut playground = section
        .div()
        .attr("tabindex='-1'")
        .attr("class='playground list_commands'");
    list_interface(&mut playground)?;
    let (_, groups) = list_get_groups(Some(test_query), Arc::new(state), Some(test_query));
    let mut list = playground.ul();
    let (width, mut available_key_classes) = character_iterator(groups.len(), "".into());
    for group in groups.into_iter() {
        list_nest(
            &mut available_key_classes,
            width,
            "".into(),
            &mut list,
            group,
            /* nesting */ 0,
        )?;
    }

    if let Some((query, links)) = query_with_links {
        writeln!(
            section,
            "If you are satisfied, write following entry into the config:"
        )?;
        writeln!(
            section.pre(),
            "{}",
            serde_json::to_string_pretty(&ConfigLinkQueryFile {
                query,
                links,
                link: None,
                exact: false
            })
            .unwrap_or_else(|err| format!("Unable to serialize config entry: {:#?}", err))
        )?;
    } else {
        writeln!(
            section,
            "Query or links is missing. Not showing what to add to config."
        )?;
    }
    Ok(())
}
