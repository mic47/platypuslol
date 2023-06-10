use std::cmp::Ordering;
pub type Score = f64;

#[derive(Clone, Debug, PartialEq)]
pub enum ParserResult<T> {
    Parsed { value: T, score: Score },
    Suggested { value: T },
}

impl<T> ParserResult<T> {
    pub fn is_parsed(&self) -> bool {
        match self {
            ParserResult::Parsed { .. } => true,
            ParserResult::Suggested { .. } => false,
        }
    }

    pub fn is_suggested(&self) -> bool {
        match self {
            ParserResult::Parsed { .. } => false,
            ParserResult::Suggested { .. } => true,
        }
    }

    pub fn score(&self) -> f64 {
        match self {
            ParserResult::Parsed { value: _, score } => *score,
            ParserResult::Suggested { value: _ } => 0.0,
        }
    }

    pub fn value(self) -> T {
        match self {
            ParserResult::Parsed { value, score: _ } => value,
            ParserResult::Suggested { value } => value,
        }
    }

    pub fn replace_with<R>(self, replacement: R) -> ParserResult<R> {
        match self {
            ParserResult::Parsed { value: _, score } => ParserResult::Parsed {
                value: replacement,
                score,
            },
            ParserResult::Suggested { value: _ } => ParserResult::Suggested { value: replacement },
        }
    }
}

pub struct AmbiguousParser<T> {
    parse: Box<dyn Fn(&str) -> Vec<(ParserResult<T>, String)>>,
}

// TODO: remove static: why it is here even?
impl<'a, T: 'a + 'static> AmbiguousParser<T> {
    pub fn any_of(parsers: Vec<AmbiguousParser<T>>) -> AmbiguousParser<T> {
        let parsers = std::rc::Rc::new(parsers);
        return AmbiguousParser {
            parse: Box::new(move |s| parsers.iter().flat_map(|parser| parser.parse(s)).collect()),
        };
    }

    pub fn replace_with<R: Clone + 'static>(self, replacement: R) -> AmbiguousParser<R> {
        let replacement = replacement.clone();
        AmbiguousParser {
            parse: Box::new(move |s| {
                self.parse(s)
                    .into_iter()
                    .map(|x| (x.0.replace_with(replacement.clone()), x.1))
                    .collect()
            }),
        }
    }
}

impl<T> AmbiguousParser<T> {
    pub fn parse(&self, input: &str) -> Vec<(ParserResult<T>, String)> {
        (self.parse)(input)
    }

    pub fn parse_all(&self, input: &str) -> Vec<T> {
        let mut items: Vec<_> = self
            .parse(input)
            .into_iter()
            .filter(|x| x.1.is_empty())
            .filter_map(|x| match x.0 {
                ParserResult::Parsed { value, score } => Some((value, score)),
                ParserResult::Suggested { .. } => None,
            })
            .collect();
        items.sort_by(|x, y| x.1.partial_cmp(&y.1).unwrap_or(Ordering::Less));
        items.into_iter().map(|x| x.0).collect()
    }

    pub fn parse_then_suggest(&self, input: &str) -> Vec<T> {
        let (mut parsed, not_suggested): (Vec<_>, Vec<_>) = self
            .parse(input)
            .into_iter()
            .filter_map(|x| if x.1.is_empty() { Some(x.0) } else { None })
            .partition(|x| x.is_parsed());
        parsed.sort_by(|x, y| x.score().partial_cmp(&y.score()).unwrap_or(Ordering::Less));
        parsed
            .into_iter()
            .map(|x| x.value())
            .chain(not_suggested.into_iter().map(|x| x.value()))
            .collect()
    }

}

impl AmbiguousParser<String> {
    pub fn string(input: String) -> AmbiguousParser<String> {
        return AmbiguousParser {
            parse: Box::new(move |s| {
                if s.starts_with(&input) {
                    vec![(
                        ParserResult::Parsed {
                            value: input.clone(),
                            score: 1.0,
                        },
                        s.split_at(input.len()).1.into(),
                    )]
                } else if input.starts_with(s) {
                    vec![(
                        ParserResult::Suggested {
                            value: input.split_at(s.len()).1.into(),
                        },
                        "".into(),
                    )]
                } else {
                    vec![]
                }
            }),
        };
    }
}

