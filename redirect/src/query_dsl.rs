use std::{
    collections::{hash_map::DefaultHasher, HashSet},
    hash::{Hash, Hasher},
};

use nfa::Regex;

#[derive(Clone, Debug)]
pub struct Substitution {
    pub name: String,
    pub field: String,
}

fn validate_braces(input: &str) -> Result<(), String> {
    let mut cnt: i64 = 0;
    for (position, char) in input.char_indices() {
        match char {
            '{' => cnt += 1,
            '}' => cnt -= 1,
            _ => (),
        }
        if cnt > 1 {
            Err(format!(
                "Unexpected open brace at character at position {}",
                position
            ))?;
        } else if cnt < 0 {
            Err(format!(
                "Unexpected close brace at character at position {}",
                position
            ))?;
        }
    }
    if cnt != 0 {
        Err("Missing closing brace!".to_string())?;
    }
    Ok(())
}

fn split_by_braces(input: &str) -> Result<Vec<&str>, String> {
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
                Err("Not matching parenthesis. This is impossible".to_string())?;
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

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum QueryToken {
    Exact(String),
    Prefix(String),
    Regex(String, Regex),
    Substitution(String, String, String),
}

impl QueryToken {
    pub fn content_hashes(tokens: &[Self]) -> Vec<u64> {
        let mut out = vec![];
        let mut s = DefaultHasher::new();
        for token in tokens {
            match token {
                QueryToken::Exact(item) => {
                    item.hash(&mut s);
                }
                QueryToken::Prefix(item) => {
                    item.hash(&mut s);
                }
                QueryToken::Regex(_, _) => {
                    "<QUERY>".hash(&mut s);
                }
                QueryToken::Substitution(name, _, _) => {
                    format!("<{}>", name).hash(&mut s);
                }
            }
            out.push(s.finish());
        }
        out
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
            | (QueryToken::Prefix(left), QueryToken::Prefix(right)) => left.cmp(right),
            (QueryToken::Regex(lefta, leftb), QueryToken::Regex(righta, rightb)) => {
                (lefta, leftb).cmp(&(righta, rightb))
            }
            (QueryToken::Substitution(la, lb, lc), QueryToken::Substitution(ra, rb, rc)) => {
                (la, lb, lc).cmp(&(ra, rb, rc))
            }
            (QueryToken::Exact(_), _) => std::cmp::Ordering::Less,
            (QueryToken::Prefix(_), _) => std::cmp::Ordering::Less,
            (QueryToken::Regex(_, _), _) => std::cmp::Ordering::Less,
            (QueryToken::Substitution(_, _, _), _) => std::cmp::Ordering::Less,
        }
    }
}

impl QueryToken {
    fn new(tokens: &[&str]) -> Result<Self, String> {
        Ok(match tokens {
            [] => Err("empty brace parameters")?,
            [item, "exact"] => Self::Exact((*item).into()),
            [item, "word"] => Self::Regex((*item).into(), Regex::new(r"\w+")?),
            [item] | [item, "query"] => Self::Regex((*item).into(), Regex::new(r".+")?),
            [item, "subst", type_, subtype] => Self::Substitution(
                if item.is_empty() {
                    (*type_).into()
                } else {
                    (*item).into()
                },
                (*type_).into(),
                (*subtype).into(),
            ),
            x => Err(format!("unable to parse brace expression {:?}", x))?,
        })
    }
}

#[derive(Clone, Debug)]
pub enum LinkToken {
    Exact(String),
    Replacement(String),
    Substitution(String, String),
}
impl LinkToken {
    fn new(tokens: &[&str]) -> Result<Self, String> {
        Ok(match tokens {
            [] => Err("empty brace parameters")?,
            [item] => Self::Replacement((*item).into()),
            [type_, subtype] => Self::Substitution((*type_).into(), (*subtype).into()),
            x => Err(format!("unable to parse brace expression {:?}", x))?,
        })
    }
}

pub fn parse_query(input: &str, exact: bool) -> Result<Vec<QueryToken>, String> {
    split_by_braces(input)?
        .into_iter()
        .flat_map(|item| {
            if let Some(braces) = parse_braces(item) {
                vec![QueryToken::new(&braces)]
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

pub fn parse_link(input: &str) -> Result<Vec<LinkToken>, String> {
    split_by_braces(input)?
        .into_iter()
        .map(|item| {
            if let Some(braces) = parse_braces(item) {
                LinkToken::new(&braces)
            } else {
                Ok(LinkToken::Exact(item.into()))
            }
        })
        .collect()
}

pub fn validate_query_with_link(
    query: &[QueryToken],
    query_str: &str,
    link: &[LinkToken],
    link_str: &str,
) -> Result<(), String> {
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
        return Err(format!(
            "Following replacements are in the link but not in query: {:?}. Query: '{}', link: '{}'",
            difference, query_str, link_str,
        ));
    }
    let link_substitution_types = link
        .iter()
        .filter_map(|x| match x {
            LinkToken::Substitution(type_, _) => Some(type_),
            _ => None,
        })
        .collect::<HashSet<_>>();
    let query_substitution_types = query
        .iter()
        .filter_map(|x| match x {
            QueryToken::Substitution(type_, _, _) => Some(type_),
            _ => None,
        })
        .collect::<HashSet<_>>();
    let difference = link_substitution_types
        .difference(&query_substitution_types)
        .collect::<Vec<_>>();
    if !difference.is_empty() {
        return Err(format!(
            "Following substitutions are in the link but not in query: {:?}. Query: '{}', link: '{}'",
            difference, query_str, link_str,
        ));
    }
    Ok(())
}
