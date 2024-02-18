/// Lighter version regex::Regex, which does only subset of operations,
/// but does not blew up wasm size.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Regex {
    Word,
    RestOfTheString,
}

#[derive(Clone, Debug)]
pub struct ReMatch<'a> {
    input: &'a str,
    start: usize,
    end: usize,
}

impl<'a> ReMatch<'a> {
    pub fn start(&self) -> usize {
        self.start
    }
    pub fn end(&self) -> usize {
        self.end
    }
    pub fn as_str(&self) -> &'a str {
        &self.input[self.start..self.end]
    }
}

impl Regex {
    pub fn new(regex: &str) -> anyhow::Result<Self> {
        match regex {
            r"\w+" => Ok(Regex::Word),
            r".+" => Ok(Regex::RestOfTheString),
            _ => Err(anyhow::anyhow!("Unsupported regex '{regex}'")),
        }
    }

    pub fn find<'a>(&self, input: &'a str) -> Option<ReMatch<'a>> {
        match self {
            Regex::Word => input.split_whitespace().next().and_then(|find| {
                if find.is_empty() {
                    None
                } else {
                    Some(ReMatch {
                        input,
                        start: 0,
                        end: find.len(),
                    })
                }
            }),
            Regex::RestOfTheString => Some(ReMatch {
                input,
                start: 0,
                end: input.len(),
            }),
        }
    }
}
