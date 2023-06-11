use std::{cmp::Ordering, sync::Arc};
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

    pub fn map<R, F: Fn(T) -> R>(self, f: F) -> ParserResult<R> {
        match self {
            ParserResult::Parsed { value, score } => ParserResult::Parsed {
                value: f(value),
                score,
            },
            ParserResult::Suggested { value } => ParserResult::Suggested { value: f(value) },
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

    pub fn sequence(self, second: AmbiguousParser<T>) -> AmbiguousParser<T> {
        return AmbiguousParser {
            parse: Box::new(move |s| {
                let state = self.parse(s);
                let mut out = vec![];
                for item in state.into_iter() {
                    for last_item in second.parse(&item.1).into_iter() {
                        out.push(match (&item.0, last_item.0) {
                            (
                                ParserResult::Parsed {
                                    value: _,
                                    score: old_score,
                                },
                                ParserResult::Parsed { value, score },
                            ) => (
                                ParserResult::Parsed {
                                    value,
                                    score: score + old_score,
                                },
                                last_item.1,
                            ),
                            (
                                ParserResult::Suggested { value: _ },
                                ParserResult::Parsed { value, score: _ },
                            ) => (ParserResult::Suggested { value }, last_item.1),
                            (_, ParserResult::Suggested { value }) => {
                                (ParserResult::Suggested { value }, last_item.1)
                            }
                        });
                    }
                }
                out
            }),
        };
    }

    pub fn chain(parsers: Vec<AmbiguousParser<T>>) -> AmbiguousParser<T> {
        let mut out: Option<AmbiguousParser<T>> = None;
        for parser in parsers.into_iter() {
            out = Some(if let Some(out) = out {
                out.sequence(parser)
            } else {
                parser
            });
        }
        // TODO: remove onwrap
        out.unwrap()
    }

    // TODO: write test
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

    // TODO: write test
    pub fn map<R: 'static>(self, f: Arc<dyn Fn(T) -> R>) -> AmbiguousParser<R> {
        AmbiguousParser {
            parse: Box::new(move |s| {
                self.parse(s)
                    .into_iter()
                    .map(|x| (x.0.map(|x| f(x)), x.1))
                    .collect()
            }),
        }
    }
}

impl<'a, T: 'a + 'static + Clone> AmbiguousParser<T> {
    pub fn empty(value: T) -> Self {
        AmbiguousParser {
            parse: Box::new(move |s| {
                vec![(
                    ParserResult::Parsed {
                        value: value.clone(),
                        score: 1.,
                    },
                    s.into(),
                )]
            }),
        }
    }

    // TODO: write test
    pub fn set_suggestion(self, suggestion: T) -> AmbiguousParser<T> {
        AmbiguousParser {
            parse: Box::new(move |x| {
                let mut already_suggested = false;
                self.parse(x)
                    .into_iter()
                    .filter_map(|x| match x.0 {
                        ParserResult::Suggested { value: _ } => {
                            if already_suggested {
                                None
                            } else {
                                already_suggested = true;
                                Some((
                                    ParserResult::Suggested {
                                        value: suggestion.clone(),
                                    },
                                    x.1,
                                ))
                            }
                        }
                        parsed => Some((parsed, x.1)),
                    })
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

    pub fn parse_and_suggest(&self, input: &str) -> (Vec<T>, Vec<T>) {
        let (mut parsed, suggested): (Vec<_>, Vec<_>) = self
            .parse(input)
            .into_iter()
            .filter_map(|x| if x.1.is_empty() { Some(x.0) } else { None })
            .partition(|x| x.is_parsed());
        parsed.sort_by(|x, y| x.score().partial_cmp(&y.score()).unwrap_or(Ordering::Less));

        (
            parsed.into_iter().map(ParserResult::value).collect(),
            suggested.into_iter().map(ParserResult::value).collect(),
        )
    }

    pub fn parse_then_suggest(&self, input: &str) -> Vec<T> {
        let (parsed, suggested) = self.parse_and_suggest(input);
        parsed.into_iter().chain(suggested.into_iter()).collect()
    }
}

impl AmbiguousParser<String> {
    pub fn string(input: &str) -> AmbiguousParser<String> {
        let input = String::from(input);
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
                            value: input.clone(),
                        },
                        "".into(),
                    )]
                } else {
                    vec![]
                }
            }),
        };
    }

    pub fn rest_of_string(suggestion: String) -> AmbiguousParser<String> {
        return AmbiguousParser {
            parse: Box::new(move |s| {
                vec![if s.is_empty() {
                    (
                        ParserResult::Suggested {
                            value: suggestion.clone(),
                        },
                        s.into(),
                    )
                } else {
                    (
                        ParserResult::Parsed {
                            value: s.into(),
                            score: 1.,
                        },
                        "".into(),
                    )
                }]
            }),
        };
    }

    pub fn word(suggestion: String) -> AmbiguousParser<String> {
        return AmbiguousParser {
            parse: Box::new(move |s| {
                if s.is_empty() {
                    vec![(
                        ParserResult::Suggested {
                            value: suggestion.clone(),
                        },
                        s.into(),
                    )]
                } else if let Some(index) = s.find(" ") {
                    let (word, rest) = s.split_at(index);
                    vec![(
                        ParserResult::Parsed {
                            value: word.into(),
                            score: 1.,
                        },
                        rest.into(),
                    )]
                } else if s.is_empty() {
                    vec![]
                } else {
                    vec![(
                        ParserResult::Parsed {
                            value: s.into(),
                            score: 1.,
                        },
                        "".into(),
                    )]
                }
            }),
        };
    }

    pub fn at_least_one_space() -> AmbiguousParser<String> {
        return AmbiguousParser {
            parse: Box::new(move |s| {
                let trimmed = s.trim_start();
                if trimmed.len() == s.len() {
                    return vec![];
                }
                return vec![(
                    ParserResult::Parsed {
                        value: " ".into(),
                        score: 1.,
                    },
                    trimmed.into(),
                )];
            }),
        };
    }

    pub fn at_least_zero_space() -> AmbiguousParser<String> {
        return AmbiguousParser {
            parse: Box::new(move |s| {
                let trimmed = s.trim_start();
                return vec![(
                    ParserResult::Parsed {
                        value: " ".into(),
                        score: 1.,
                    },
                    trimmed.into(),
                )];
            }),
        };
    }

    pub fn non_empty_prefixes(word: &str) -> AmbiguousParser<String> {
        Self::any_of(
            (1..word.len() + 1)
                .map(|x| AmbiguousParser::string(word[..x].into()).replace_with(word.into()))
                .collect(),
        )
        .set_suggestion(word.into())
    }
}

#[test]
fn test_string() {
    let parser = AmbiguousParser::string("foo");
    assert_eq!(
        parser.parse_and_suggest("foo"),
        (vec!["foo".into()], vec![],)
    );
    assert_eq!(parser.parse_and_suggest(""), (vec![], vec!["foo".into()],));
    assert_eq!(parser.parse_and_suggest("f"), (vec![], vec!["foo".into()],));
}

#[test]
fn test_non_empty_prefixes() {
    let parser = AmbiguousParser::non_empty_prefixes("foo");
    assert_eq!(
        parser.parse_and_suggest("foo"),
        (vec!["foo".into()], vec![],)
    );
    assert_eq!(parser.parse_and_suggest(""), (vec![], vec!["foo".into()],));
    assert_eq!(parser.parse_and_suggest("f"), (vec!["foo".into()], vec!["foo".into()],));
}

#[test]
fn test_at_least_zero_space() {
    let parser = AmbiguousParser::at_least_zero_space();
    assert_eq!(
        parser.parse_and_suggest(""),
        (vec![" ".into()], vec![]),
    );
    assert_eq!(
        parser.parse_and_suggest(" "),
        (vec![" ".into()], vec![]),
    );
    assert_eq!(
        parser.parse_and_suggest("    "),
        (vec![" ".into()], vec![]),
    );
    assert_eq!(
        parser.parse_and_suggest("p"),
        (vec![], vec![]),
    );
}

#[test]
fn test_at_least_one_space() {
    let parser = AmbiguousParser::at_least_one_space();
    assert_eq!(
        parser.parse_and_suggest(""),
        (vec![], vec![]),
    );
    assert_eq!(
        parser.parse_and_suggest(" "),
        (vec![" ".into()], vec![]),
    );
    assert_eq!(
        parser.parse_and_suggest("    "),
        (vec![" ".into()], vec![]),
    );
    assert_eq!(
        parser.parse_and_suggest("p"),
        (vec![], vec![]),
    );
}

#[test]
fn test_word() {
    let parser = AmbiguousParser::word("foo".into());
    assert_eq!(
        parser.parse_and_suggest("bar"),
        (vec!["bar".into()], vec![],)
    );
    assert_eq!(parser.parse_and_suggest(""), (vec![], vec!["foo".into()],));
    assert_eq!(parser.parse_and_suggest("f"), (vec!["f".into()], vec![],));
    assert_eq!(parser.parse_and_suggest("f b"), (vec![], vec![],));
}

#[test]
fn test_rest_of_string() {
    let parser = AmbiguousParser::rest_of_string("foo".into());
    assert_eq!(
        parser.parse_and_suggest("bar"),
        (vec!["bar".into()], vec![],)
    );
    assert_eq!(parser.parse_and_suggest(""), (vec![], vec!["foo".into()],));
    assert_eq!(parser.parse_and_suggest("f"), (vec!["f".into()], vec![],));
    assert_eq!(parser.parse_and_suggest("f b"), (vec!["f b".into()], vec![],));
}

#[test]
fn test_empty() {
    let parser = AmbiguousParser::<String>::empty("foo".into());
    assert_eq!(
        parser.parse_and_suggest("bar"),
        (vec![], vec![],)
    );
    assert_eq!(parser.parse_and_suggest(""), (vec!["foo".into()], vec![],));
}

/*
#[test]
fn test_sequence() {
    let parser = AmbiguousParser::sequence(
        AmbiguousParser::string("foo".into()),
        AmbiguousParser::string("bar".into()),
    );
    assert_eq!(
        parser.parse_and_suggest(""),
        (vec![], vec!["foobar".into()],)
    );
    assert_eq!(parser.parse_and_suggest(""), (vec![], vec!["foo".into()],));
    assert_eq!(parser.parse_and_suggest("f"), (vec!["f".into()], vec![],));
    assert_eq!(parser.parse_and_suggest("f b"), (vec![], vec![],));
}

#[test]
fn test_chain() {
    let parser = AmbiguousParser::chain(vec![
        AmbiguousParser::string("foo".into()),
        AmbiguousParser::at_least_one_space(),
        AmbiguousParser::string("bar".into()),
    ]);
    assert_eq!(
        parser.parse_and_suggest("foo"),
        (vec![], vec!["foo bar".into()],)
    );
    assert_eq!(parser.parse_and_suggest(""), (vec![], vec!["foo".into()],));
    assert_eq!(parser.parse_and_suggest("f"), (vec!["f".into()], vec![],));
    assert_eq!(parser.parse_and_suggest("f b"), (vec![], vec![],));
}
*/
