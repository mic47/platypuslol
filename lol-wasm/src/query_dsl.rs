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

#[derive(Clone, Debug)]
pub enum QueryToken {
    Exact(String),
    Prefix(String),
    Regex(String, regex::Regex),
    Substitution(String, String, String),
}

impl QueryToken {
    fn new(tokens: &[&str]) -> Result<Self, String> {
        Ok(match tokens {
            [] => Err("empty brace parameters")?,
            [item, "exact"] => Self::Exact((*item).into()),
            [item, "word"] => Self::Regex(
                (*item).into(),
                regex::Regex::new(r"\w+").map_err(|x| x.to_string())?,
            ),
            [item] | [item, "query"] => Self::Regex(
                (*item).into(),
                regex::Regex::new(r".+").map_err(|x| x.to_string())?,
            ),
            [item, "regex", regex] => Self::Regex(
                (*item).into(),
                regex::Regex::new(regex).map_err(|x| x.to_string())?,
            ),
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

pub fn parse_query(input: &str) -> Result<Vec<QueryToken>, String> {
    split_by_braces(input)?
        .into_iter()
        .flat_map(|item| {
            if let Some(braces) = parse_braces(item) {
                vec![QueryToken::new(&braces)]
            } else {
                item.split_whitespace()
                    .map(|x| Ok(QueryToken::Prefix(x.into())))
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
