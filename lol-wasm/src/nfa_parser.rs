use std::collections::{HashMap, VecDeque};

type Length = usize;
type NodeIndex = usize;

#[derive(Clone, Debug)]
pub struct Node<T> {
    pub value: Option<T>,
    edges: HashMap<Length, HashMap<String, Vec<NodeIndex>>>,
}

impl<T: Clone> Node<T> {
    fn shift(&self, shift: usize) -> Self {
        Node {
            value: self.value.clone(),
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
            .filter(|x| x.1.value.is_some())
            .map(|x| x.0)
            .collect::<Vec<_>>();
        let mut shift = ret.nodes.len();
        for nfa in nfas[1..].into_iter() {
            let root = shift + nfa.root;
            leafs.iter().for_each(|index| {
                // TODO: we are loosing this
                ret.nodes[*index].value = None;
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
                if node.value.is_some() {
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
                value: None,
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
                value: None,
                edges: HashMap::from([(input.len(), HashMap::from([(input.into(), vec![1])]))]),
            },
            Node {
                value: Some(()),
                edges: HashMap::default(),
            },
        ];
        NFA { nodes, root: 0 }
    }

    pub fn match_non_empty_prefixes(input: &str) -> NFA<()> {
        let nodes = vec![
            Node {
                value: None,
                edges: (1..input.len() + 1)
                    .map(|len| (len, HashMap::from([(input[0..len].into(), vec![1])])))
                    .collect(),
            },
            Node {
                value: Some(()),
                edges: HashMap::default(),
            },
        ];
        NFA { nodes, root: 0 }
    }

    pub fn match_zero_or_more_spaces() -> NFA<()> {
        let nodes = vec![Node {
            value: Some(()),
            edges: HashMap::from([(1, HashMap::from([(" ".into(), vec![0])]))]),
        }];
        NFA { nodes, root: 0 }
    }

    pub fn match_one_or_more_spaces() -> NFA<()> {
        let nodes = vec![
            Node {
                value: None,
                edges: HashMap::from([(1, HashMap::from([(" ".into(), vec![1])]))]),
            },
            Node {
                value: Some(()),
                edges: HashMap::from([(1, HashMap::from([(" ".into(), vec![1])]))]),
            },
        ];
        NFA { nodes, root: 0 }
    }
}

impl<T> NFA<T> {
    pub fn nothing() -> NFA<T> {
        NFA {
            nodes: vec![Node {
                value: None,
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

trait Parser<T> {
    fn parse<'a, 'b>(&'a self, input: &'b str) -> Vec<(&'a T, &'b str)>;
}

impl<T: std::fmt::Debug> NFA<T> {
    pub fn parse_full<'a>(&'a self, input: &str) -> Vec<&'a T> {
        self.parse(input)
            .into_iter()
            .filter_map(|x| if x.1.is_empty() { Some(x.0) } else { None })
            .collect()
    }
}

impl<T: std::fmt::Debug> Parser<T> for NFA<T> {
    fn parse<'a, 'b>(&'a self, input: &'b str) -> Vec<(&'a T, &'b str)> {
        println!("self: {:#?}", self);
        let mut state = VecDeque::from([(&self.nodes[self.root], input)]);
        println!("state: {:#?}", state);
        let mut output = vec![];
        while let Some((node, string)) = state.pop_front() {
            println!("state: {:#?}, node: {:#?}, string: {}", state, node, string);
            if let Some(ref value) = node.value {
                output.push((value, string));
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
