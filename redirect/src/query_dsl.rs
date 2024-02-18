use std::{
    cmp::Ordering,
    collections::{hash_map::DefaultHasher, HashMap, HashSet},
    hash::{Hash, Hasher},
};

use anyhow::Context;

use nfa::Regex;

#[derive(Clone, Debug)]
pub struct Substitution {
    pub name: String,
    pub field: String,
}

fn validate_braces(input: &str) -> anyhow::Result<()> {
    let mut cnt: i64 = 0;
    for (position, char) in input.char_indices() {
        match char {
            '{' => cnt += 1,
            '}' => cnt -= 1,
            _ => (),
        }
        if cnt > 1 {
            Err(anyhow::anyhow!(
                "Unexpected open brace at character at position {}",
                position
            ))?;
        } else if cnt < 0 {
            Err(anyhow::anyhow!(
                "Unexpected close brace at character at position {}",
                position
            ))?;
        }
    }
    if cnt != 0 {
        Err(anyhow::anyhow!("Missing closing brace!"))?;
    }
    Ok(())
}

fn split_by_braces(input: &str) -> anyhow::Result<Vec<&str>> {
    validate_braces(input)?;
    let mut input = input;
    let mut output = vec![];
    loop {
        if let Some(index) = input.find('{') {
            let (prefix, suffix) = input.split_at(index);
            output.push(prefix);
            if let Some(mut index) = suffix.find('}') {
                index += 1;
                while !suffix.is_char_boundary(index) {
                    index += 1
                }
                let (prefix, suffix) = suffix.split_at(index);
                output.push(prefix);
                input = suffix;
            } else {
                Err(anyhow::anyhow!(
                    "Not matching parenthesis. This is impossible"
                ))?;
            }
        } else {
            output.push(input);
            break;
        }
    }
    Ok(output)
}

fn parse_braces(input: &str) -> Option<Vec<&str>> {
    if input.starts_with('{') {
        Some(
            input
                .trim_start_matches('{')
                .trim_end_matches('}')
                .split(':')
                .collect(),
        )
    } else {
        None
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum QueryToken {
    Exact(String),
    Prefix(String),
    Regex(String, Regex),
    Substitution {
        name: String,
        type_: String,
        subtype: String,
    },
}

impl QueryToken {
    pub fn content_hashes(
        tokens: &[Self],
        matches: &HashMap<String, String>,
        substitutions: &HashMap<String, HashMap<String, String>>,
        default_replacement: &Option<String>,
    ) -> Vec<u64> {
        let mut out = vec![];
        let mut s = DefaultHasher::new();
        for token in tokens {
            token
                .to_description(matches, substitutions, default_replacement, false)
                .hash(&mut s);
            out.push(s.finish());
        }
        out
    }

    pub fn to_description(
        &self,
        matches: &HashMap<String, String>,
        substitutions: &HashMap<String, HashMap<String, String>>,
        default_replacement: &Option<String>,
        show_variable: bool,
    ) -> String {
        match self {
            QueryToken::Exact(data) => data.clone(),
            QueryToken::Prefix(data) => data.clone(),
            QueryToken::Regex(replacement, _) => {
                if let Some(replacement) = matches.get(replacement) {
                    // TODO: we want to html escape this in better way. Probably in javascript
                    // even?
                    replacement.clone().replace(' ', "+")
                } else {
                    default_replacement.clone().unwrap_or("<query>".into())
                }
            }
            QueryToken::Substitution {
                type_,
                name: _,
                subtype,
            } => {
                if let Some(replacement) = substitutions.get(type_).and_then(|x| x.get(subtype)) {
                    if show_variable {
                        format!("{}={}", type_.clone(), replacement.clone())
                    } else {
                        replacement.clone()
                    }
                } else {
                    format!("<{}>", type_)
                }
            }
        }
    }
}

impl PartialOrd for QueryToken {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}
impl Ord for QueryToken {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match (self, other) {
            (QueryToken::Exact(left), QueryToken::Exact(right))
            | (QueryToken::Exact(left), QueryToken::Prefix(right))
            | (QueryToken::Prefix(left), QueryToken::Exact(right))
            | (QueryToken::Prefix(left), QueryToken::Prefix(right)) => {
                let result = left.cmp(right);
                match result {
                    std::cmp::Ordering::Equal => match (self, other) {
                        (QueryToken::Exact(_), QueryToken::Exact(_)) => result,
                        (QueryToken::Prefix(_), QueryToken::Prefix(_)) => result,
                        (QueryToken::Exact(_), QueryToken::Prefix(_)) => Ordering::Less,
                        (QueryToken::Prefix(_), QueryToken::Exact(_)) => Ordering::Greater,
                        _ => unreachable!(),
                    },
                    _ => result,
                }
            }
            (QueryToken::Regex(lefta, leftb), QueryToken::Regex(righta, rightb)) => {
                (lefta, leftb).cmp(&(righta, rightb))
            }
            (
                QueryToken::Substitution {
                    name: la,
                    type_: lb,
                    subtype: lc,
                },
                QueryToken::Substitution {
                    name: ra,
                    type_: rb,
                    subtype: rc,
                },
            ) => (la, lb, lc).cmp(&(ra, rb, rc)),
            (QueryToken::Exact(_), _) => Ordering::Less,
            (_, QueryToken::Exact(_)) => Ordering::Greater,
            (QueryToken::Prefix(_), _) => Ordering::Less,
            (_, QueryToken::Prefix(_)) => Ordering::Greater,
            (QueryToken::Regex(_, _), _) => Ordering::Less,
            (_, QueryToken::Regex(_, _)) => Ordering::Greater,
        }
    }
}

impl QueryToken {
    fn new(tokens: &[&str]) -> anyhow::Result<Self> {
        Ok(match tokens {
            [] | [""] => Err(anyhow::anyhow!("empty brace parameters"))?,
            [item, "exact"] => Self::Exact((*item).into()),
            [item, "word"] => Self::Regex((*item).into(), Regex::new(r"\w+")?),
            [item] | [item, "query"] => Self::Regex((*item).into(), Regex::new(r".+")?),
            [item, "subst", type_, subtype] => Self::Substitution {
                name: if item.is_empty() {
                    (*type_).into()
                } else {
                    (*item).into()
                },
                type_: (*type_).into(),
                subtype: (*subtype).into(),
            },
            x => Err(anyhow::anyhow!("unable to parse brace expression {:?}", x))?,
        })
    }
}

#[derive(Clone, Debug)]
pub enum LinkToken {
    Exact(String),
    Replacement(String),
    Substitution { name: String, subtype: String },
}
impl LinkToken {
    fn new(tokens: &[&str]) -> anyhow::Result<Self> {
        Ok(match tokens {
            [] | [""] => Err(anyhow::anyhow!("empty brace parameters"))?,
            [item] => Self::Replacement((*item).into()),
            [name, subtype] => Self::Substitution {
                name: (*name).into(),
                subtype: (*subtype).into(),
            },
            x => Err(anyhow::anyhow!("unable to parse brace expression {:?}", x))?,
        })
    }
}

pub fn parse_query(input: &str, exact: bool) -> anyhow::Result<Vec<QueryToken>> {
    split_by_braces(input)?
        .into_iter()
        .flat_map(|item| {
            if let Some(braces) = parse_braces(item) {
                vec![QueryToken::new(&braces)
                    .with_context(|| format!("Invalid brace token: '{}'", item))]
            } else {
                let constructor = if exact {
                    QueryToken::Exact
                } else {
                    QueryToken::Prefix
                };
                item.split_whitespace()
                    .map(|x| Ok(constructor(x.into())))
                    .collect()
            }
        })
        .collect()
}

pub fn parse_link(input: &str) -> anyhow::Result<Vec<LinkToken>> {
    split_by_braces(input)?
        .into_iter()
        .map(|item| {
            if let Some(braces) = parse_braces(item) {
                LinkToken::new(&braces).with_context(|| format!("Invalid brace token: '{}'", item))
            } else {
                Ok(LinkToken::Exact(item.into()))
            }
        })
        .collect()
}

pub fn validate_query_and_link_with_substitutions(
    query: &[QueryToken],
    link: &[LinkToken],
    substitutions: &HashMap<String, Vec<HashMap<String, String>>>,
) -> anyhow::Result<()> {
    let type_to_subtype = substitutions
        .iter()
        .map(|(k, v)| (k, v.iter().flat_map(|x| x.keys()).collect::<HashSet<_>>()))
        .collect::<HashMap<_, _>>();
    let mut name_to_type: HashMap<&String, &String> = HashMap::new();
    for token in query {
        match token {
            QueryToken::Exact(_) => {}
            QueryToken::Prefix(_) => {}
            QueryToken::Regex(_, _) => {}
            QueryToken::Substitution {
                name,
                type_,
                subtype,
            } => {
                name_to_type.insert(name, type_);
                if let Some(subtypes) = type_to_subtype.get(type_) {
                    if !subtypes.contains(subtype) {
                        Err(anyhow::anyhow!(
                            "Substitution '{}' does not have '{}' in query token '{:?}'. Valid values are {:?}",
                            type_,
                            subtype,
                            token,
                            subtypes,
                        ))?
                    }
                } else {
                    Err(anyhow::anyhow!(
                        "Invalid substitution '{}' in query token '{:?}'. Valid substitutions are {:?}",
                        type_,
                        token,
                        type_to_subtype.keys(),
                    ))?
                }
            }
        }
    }
    for token in link {
        match token {
            LinkToken::Exact(_) => {}
            LinkToken::Replacement(_) => {}
            LinkToken::Substitution { name, subtype } => {
                let type_ = name_to_type.get(name).ok_or_else(|| anyhow::anyhow!(
                    "There is no substitution named '{}' in the query. Existing substitutions are {:?}",
                    name,
                    name_to_type.keys(),
                ))?;
                if let Some(subtypes) = type_to_subtype.get(*type_) {
                    if !subtypes.contains(subtype) {
                        Err(anyhow::anyhow!(
                            "Substitution '{}' does not have '{}' in link token '{:?}'. Valid values are {:?}",
                            type_,
                            subtype,
                            token,
                            subtypes,
                        ))?
                    }
                } else {
                    Err(anyhow::anyhow!(
                        "Invalid substitution '{}' in link token '{:?}'. Valid substitutions are {:?}",
                        type_,
                        token,
                        type_to_subtype.keys(),
                    ))?
                }
            }
        }
    }
    Ok(())
}

pub fn validate_query_with_link(
    query: &[QueryToken],
    query_str: &str,
    link: &[LinkToken],
    link_str: &str,
) -> anyhow::Result<()> {
    let link_replacement_types = link
        .iter()
        .filter_map(|x| match x {
            LinkToken::Replacement(data) => Some(data),
            _ => None,
        })
        .collect::<HashSet<_>>();
    let query_replacement_types = query
        .iter()
        .filter_map(|x| match x {
            QueryToken::Regex(data, _) => Some(data),
            _ => None,
        })
        .collect::<HashSet<_>>();
    let difference = link_replacement_types
        .difference(&query_replacement_types)
        .collect::<Vec<_>>();
    if !difference.is_empty() {
        return Err(anyhow::anyhow!(
            "Following replacements are in the link but not in query: {:?}. Query: '{}', link: '{}'",
            difference, query_str, link_str,
        ));
    }
    let link_substitution_names = link
        .iter()
        .filter_map(|x| match x {
            LinkToken::Substitution { name, subtype: _ } => Some(name),
            _ => None,
        })
        .collect::<HashSet<_>>();
    let query_substitution_names = query
        .iter()
        .filter_map(|x| match x {
            QueryToken::Substitution {
                name,
                type_: _,
                subtype: _,
            } => Some(name),
            _ => None,
        })
        .collect::<HashSet<_>>();
    let difference = link_substitution_names
        .difference(&query_substitution_names)
        .collect::<Vec<_>>();
    if !difference.is_empty() {
        return Err(anyhow::anyhow!(
            "Following substitutions are in the link but not in query: {:?}. Query: '{}', link: '{}'",
            difference, query_str, link_str,
        ));
    }
    Ok(())
}
