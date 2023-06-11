use std::collections::{HashMap, VecDeque, HashSet};

type Length = usize;
type NodeIndex = usize;

#[derive(Clone, Debug)]
pub struct Node<T> {
    pub payload: Option<T>,
    pub is_final: bool,
    pub edges: HashMap<Length, HashMap<String, Vec<NodeIndex>>>,
}

impl<T> Node<T> {
    fn map<R, F: Fn(&T) -> R>(&self, f: &F) -> Node<R> {
        Node {
            payload: match self.payload {
                Some(ref p) => Some(f(p)),
                None => None,
            },
            is_final: self.is_final,
            edges: self.edges.clone(),
        }
    }
}

impl<T: Clone> Node<T> {
    fn shift(&self, shift: usize) -> Self {
        Node {
            payload: self.payload.clone(),
            is_final: self.is_final,
            edges: self
                .edges
                .iter()
                .map(|(k, v)| {
                    (
                        *k,
                        v.iter()
                            .map(|(k, v)| (k.clone(), v.iter().map(|x| x + shift).collect()))
                            .collect(),
                    )
                })
                .collect(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct NFA<T> {
    nodes: Vec<Node<T>>,
    pub root: NodeIndex,
}

impl<T> NFA<T> {
    pub fn map<R, F: Fn(&T) -> R>(&self, f: &F) -> NFA<R> {
        NFA {
            nodes: self.nodes.iter().map(|x| x.map(f)).collect(),
            root: self.root,
        }
    }
}

impl<T: Clone> NFA<T> {
    pub fn chain(nfas: &[NFA<T>]) -> NFA<T> {
        if nfas.is_empty() {
            return Self::nothing();
        }
        let mut ret = nfas[0].clone();
        let mut leafs = ret
            .nodes
            .iter()
            .enumerate()
            .filter(|x| x.1.is_final)
            .map(|x| x.0)
            .collect::<Vec<_>>();
        let mut shift = ret.nodes.len();
        for nfa in nfas[1..].into_iter() {
            let root = shift + nfa.root;
            leafs.iter().for_each(|index| {
                ret.nodes[*index].is_final = false;
                ret.nodes[*index]
                    .edges
                    .entry(0)
                    .or_default()
                    .entry("".into())
                    .or_default()
                    .push(root);
            });
            leafs.clear();
            for node in nfa.nodes.iter() {
                let node = node.shift(shift);
                if node.is_final {
                    leafs.push(ret.nodes.len());
                }
                ret.nodes.push(node);
            }
            shift = ret.nodes.len();
        }
        ret
    }

    pub fn any_of(nfas: &[NFA<T>]) -> NFA<T> {
        if nfas.is_empty() {
            return Self::nothing();
        }
        let mut ret = NFA {
            nodes: vec![Node {
                payload: None,
                is_final: false,
                edges: HashMap::default(),
            }],
            root: 0,
        };
        let mut shift = ret.nodes.len();
        for nfa in nfas.into_iter() {
            let root = shift + nfa.root;
            ret.nodes[0]
                .edges
                .entry(0)
                .or_default()
                .entry("".into())
                .or_default()
                .push(root);
            for node in nfa.nodes.iter() {
                let node = node.shift(shift);
                ret.nodes.push(node);
            }
            shift = ret.nodes.len();
        }
        ret
    }
}

impl NFA<()> {
    pub fn match_string(input: &str) -> NFA<()> {
        let nodes = vec![
            Node {
                payload: None,
                is_final: false,
                edges: HashMap::from([(input.len(), HashMap::from([(input.into(), vec![1])]))]),
            },
            Node {
                payload: Some(()),
                is_final: true,
                edges: HashMap::default(),
            },
        ];
        NFA { nodes, root: 0 }
    }

    pub fn match_non_empty_prefixes(input: &str) -> NFA<()> {
        let nodes = vec![
            Node {
                payload: None,
                is_final: false,
                edges: (1..input.len() + 1)
                    .map(|len| (len, HashMap::from([(input[0..len].into(), vec![1])])))
                    .collect(),
            },
            Node {
                payload: Some(()),
                is_final: true,
                edges: HashMap::default(),
            },
        ];
        NFA { nodes, root: 0 }
    }

    pub fn match_zero_or_more_spaces() -> NFA<()> {
        let nodes = vec![Node {
            payload: Some(()),
            is_final: true,
            edges: HashMap::from([(1, HashMap::from([(" ".into(), vec![0])]))]),
        }];
        NFA { nodes, root: 0 }
    }

    pub fn match_one_or_more_spaces() -> NFA<()> {
        let nodes = vec![
            Node {
                payload: None,
                is_final: false,
                edges: HashMap::from([(1, HashMap::from([(" ".into(), vec![1])]))]),
            },
            Node {
                payload: Some(()),
                is_final: true,
                edges: HashMap::from([(1, HashMap::from([(" ".into(), vec![1])]))]),
            },
        ];
        NFA { nodes, root: 0 }
    }

    pub fn rest_of_string(x: String) -> NFA<()> {
        Self::nothing()
    }
    pub fn word(x: String) -> NFA<()> {
        Self::nothing()
    }
}

impl<T> NFA<T> {
    pub fn nothing() -> NFA<T> {
        NFA {
            nodes: vec![Node {
                payload: None,
                is_final: false,
                edges: HashMap::default(),
            }],
            root: 0,
        }
    }

    fn node_parse<'a, 'b>(
        &'a self,
        node_self: &Node<T>,
        input: &'b str,
    ) -> Vec<(&'a Node<T>, &'b str)> {
        let length = input.len();
        node_self
            .edges
            .iter()
            .filter(|(len, _)| len <= &&length)
            .flat_map(|(len, hm)| {
                if let Some(edges) = hm.get(&input[..*len]) {
                    let rest = input.split_at(*len).1;
                    edges
                        .into_iter()
                        .map(|index| (&self.nodes[*index], rest))
                        .collect()
                } else {
                    vec![]
                }
            })
            .collect()
    }
}

pub trait Parser<T> {
    fn parse<'a, 'b>(&'a self, input: &'b str) -> Vec<(&'a T, &'b str)>;
}

impl<T: std::fmt::Debug> NFA<T> {
    pub fn parse_full<'a>(&'a self, input: &str) -> Vec<&'a T> {
        self.parse(input)
            .into_iter()
            .filter_map(|x| if x.1.is_empty() { Some(x.0) } else { None })
            .collect()
    }

    pub fn parse_full_and_suggest<'a, 'b>(&'a self, input: &'b str) -> (Vec<(&'a T, &'b str)>, Vec<(String, &'a Option<T>)>) {
        // TODO: collect payloads
        //println!("self: {:#?}", self);
        let mut state = VecDeque::from([(&self.nodes[self.root], input)]);
        //println!("state: {:#?}", state);
        let mut output = vec![];
        let mut suggestion_states: VecDeque<(_, Vec<&str>)> = VecDeque::from([]);
        while let Some((node, string)) = state.pop_front() {
            //println!("state: {:#?}, node: {:#?}, string: {}", state, node, string);
            if string.is_empty() {
                suggestion_states.push_back((node, vec![]))
            }
            if node.is_final {
                if let Some(ref payload) = node.payload {
                    output.push((payload, string));
                } else {
                    // TODO: what is it's missing, or have value and not final...
                }
            }
            state.extend(self.node_parse(node, string));
        }
        // BFS for finding suggestions
        // TODO: suggestions are ugly here, i.e. for shortcuts, we would like to have something
        // else for suggestions.
        let mut suggestions = vec![];
        let mut visited: HashSet<NodeIndex> = Default::default();
        while let Some((node, suggestion)) = suggestion_states.pop_front() {
            if node.is_final {
                suggestions.push((format!("{}{}", input, suggestion.join("")), &node.payload))
            }
            for (text, target_nodes) in node.edges.values().flat_map(|x| x.iter()) {
                for target_node in target_nodes.iter().filter(|x| visited.insert(**x)) {
                    let mut suggestion = suggestion.clone();
                    suggestion.push(text);
                    suggestion_states.push_back((
                        &self.nodes[*target_node],
                        suggestion,
                    ))
                }
            }
        }

        (output.into_iter().filter(|x| x.1.is_empty()).collect(), suggestions)
    }
}

impl<T: std::fmt::Debug> Parser<T> for NFA<T> {
    fn parse<'a, 'b>(&'a self, input: &'b str) -> Vec<(&'a T, &'b str)> {
        // TODO: collect payloads
        //println!("self: {:#?}", self);
        let mut state = VecDeque::from([(&self.nodes[self.root], input)]);
        //println!("state: {:#?}", state);
        let mut output = vec![];
        while let Some((node, string)) = state.pop_front() {
            //println!("state: {:#?}, node: {:#?}, string: {}", state, node, string);
            if node.is_final {
                if let Some(ref payload) = node.payload {
                    output.push((payload, string));
                } else {
                    // TODO: what is it's missing, or have value and not final...
                }
            }
            state.extend(self.node_parse(node, string));
        }
        output
    }
}

#[test]
fn test_match_string() {
    let parser = NFA::match_string("foo");
    assert_eq!(parser.parse("foo"), vec![(&(), "")],);
    assert_eq!(parser.parse("foobar"), vec![(&(), "bar")],);
    assert_eq!(parser.parse(""), vec![]);
    assert_eq!(parser.parse("f"), vec![]);
}

#[test]
fn test_non_empty_prefixes() {
    let parser = NFA::match_non_empty_prefixes("foo");
    let empty: Vec<&()> = vec![];
    assert_eq!(parser.parse("f"), vec![(&(), "")],);
    assert_eq!(parser.parse_full("foo"), vec![&()]);
    assert_eq!(parser.parse_full("fo"), vec![&()],);
    assert_eq!(parser.parse_full("foobar"), empty);
    assert_eq!(parser.parse_full("fobar"), empty);
    assert_eq!(parser.parse("fbar"), vec![(&(), "bar")],);
    assert_eq!(parser.parse("obar"), vec![],);
    assert_eq!(parser.parse("bar"), vec![],);
    assert_eq!(parser.parse(""), vec![]);
}

#[test]
fn test_chain_2_elements() {
    let parser = NFA::chain(&[
        NFA::match_non_empty_prefixes("foo"),
        NFA::match_non_empty_prefixes("bar"),
    ]);
    let empty: Vec<&()> = vec![];
    let found = vec![&()];
    assert_eq!(parser.parse_full("f"), empty);
    assert_eq!(parser.parse_full("fb"), found);
    assert_eq!(parser.parse_full("fobar"), found);
    assert_eq!(parser.parse_full("foobar"), found);
    assert_eq!(parser.parse_full("fooar"), empty);
    assert_eq!(parser.parse_full("foobar_extra"), empty);
    assert_eq!(parser.parse("foob_extra"), vec![(&(), "_extra")]);
}

#[test]
fn test_chain_3_elements_with_non_zero_space() {
    let parser = NFA::chain(&[
        NFA::match_non_empty_prefixes("foo"),
        NFA::match_one_or_more_spaces(),
        NFA::match_non_empty_prefixes("bar"),
    ]);
    let empty: Vec<&()> = vec![];
    let found = vec![&()];
    assert_eq!(parser.parse_full("f"), empty);
    assert_eq!(parser.parse_full("f b"), found);
    assert_eq!(parser.parse_full("fb"), empty);
    assert_eq!(parser.parse_full("fo bar"), found);
    assert_eq!(parser.parse_full("fobar"), empty);
    assert_eq!(parser.parse_full("foo bar"), found);
    assert_eq!(parser.parse_full("fooar"), empty);
    assert_eq!(parser.parse_full("foobar_extra"), empty);
    assert_eq!(parser.parse("foo b_extra"), vec![(&(), "_extra")]);
    assert_eq!(parser.parse("foob_extra"), vec![]);
}

#[test]
fn test_chain_3_elements_with_zero_space() {
    let parser = NFA::chain(&[
        NFA::match_non_empty_prefixes("foo"),
        NFA::match_zero_or_more_spaces(),
        NFA::match_non_empty_prefixes("bar"),
    ]);
    let empty: Vec<&()> = vec![];
    let found = vec![&()];
    assert_eq!(parser.parse_full("f"), empty);
    assert_eq!(parser.parse_full("f b"), found);
    assert_eq!(parser.parse_full("fb"), found);
    assert_eq!(parser.parse_full("fo bar"), found);
    assert_eq!(parser.parse_full("fobar"), found);
    assert_eq!(parser.parse_full("foo bar"), found);
    assert_eq!(parser.parse_full("fooar"), empty);
    assert_eq!(parser.parse_full("foobar_extra"), empty);
    assert_eq!(parser.parse("foo b_extra"), vec![(&(), "_extra")]);
    assert_eq!(parser.parse("foob_extra"), vec![(&(), "_extra")]);
}

#[test]
fn test_any_of() {
    let parser = NFA::any_of(&[
        NFA::match_non_empty_prefixes("foo"),
        NFA::match_non_empty_prefixes("feee"),
        NFA::match_string("bar"),
    ]);
    let empty: Vec<&()> = vec![];
    let found = vec![&()];
    assert_eq!(parser.parse_full("f"), vec![&(), &()]);
    assert_eq!(parser.parse_full("b"), empty);
    assert_eq!(parser.parse_full("bar"), found);
    assert_eq!(parser.parse_full("fe"), found);

    assert_eq!(parser.parse("f"), vec![(&(), ""), (&(), "")]);

    assert_eq!(parser.parse("fbar"), vec![(&(), "bar"), (&(), "bar")]);
    assert_eq!(parser.parse(""), vec![]);
}
