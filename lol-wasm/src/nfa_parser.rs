use std::collections::{HashMap, HashSet, VecDeque};

type Length = usize;
type NodeIndex = usize;

#[derive(Clone, Debug)]
pub struct WithIdentifier<T> {
    pub identifier: String,
    pub payload: T,
}

#[derive(Clone, Debug)]
pub enum Trace<T> {
    Edge(WithIdentifier<EdgeData>),
    Node(T),
}

#[derive(Clone, Debug)]
pub struct RegExEdge {
    expression: regex::Regex,
    suggestion: String,
    target: Vec<NodeIndex>,
    identifier: String,
}

#[derive(Clone, Debug)]
pub struct SubstitutionEdge {
    needles: Vec<(String, HashMap<String, String>)>,
    suggestion: String,
    target: Vec<NodeIndex>,
    identifier: String,
}

#[derive(Clone, Debug)]
pub struct Node<T> {
    payload: Option<T>,
    is_final: bool,
    normal_edges: HashMap<Length, HashMap<String, Vec<NodeIndex>>>,
    regex_edges: Vec<RegExEdge>,
    substitution_edges: Vec<SubstitutionEdge>,
}

impl<T> Default for Node<T> {
    fn default() -> Self {
        Node {
            payload: Default::default(),
            is_final: false,
            normal_edges: Default::default(),
            regex_edges: Default::default(),
            substitution_edges: Default::default(),
        }
    }
}

#[derive(Clone, Debug)]
pub enum EdgeData {
    Match(String),
    Substitution(HashMap<String, String>),
}

impl<T> Node<T> {
    pub fn with_payload_and_final(mut self, payload: T) -> Self {
        self.payload = Some(payload);
        self.is_final = true;
        self
    }

    fn map<R, F: Fn(&T) -> R>(&self, f: &F) -> Node<R> {
        Node {
            payload: match self.payload {
                Some(ref p) => Some(f(p)),
                None => None,
            },
            is_final: self.is_final,
            normal_edges: self.normal_edges.clone(),
            regex_edges: self.regex_edges.clone(),
            substitution_edges: self.substitution_edges.clone(),
        }
    }

    pub fn add_normal_edge(&mut self, value: String, index: NodeIndex) {
        self.normal_edges
            .entry(value.len())
            .or_default()
            .entry(value)
            .or_default()
            .push(index);
    }

    pub fn with_normal_edge(mut self, value: String, index: NodeIndex) -> Self {
        self.add_normal_edge(value, index);
        self
    }

    pub fn with_regex_edge(mut self, edge: RegExEdge) -> Self {
        self.regex_edges.push(edge);
        self
    }

    pub fn with_substitution_edge(mut self, edge: SubstitutionEdge) -> Self {
        self.substitution_edges.push(edge);
        self
    }

    pub fn with_normal_edges(mut self, edges: &[(&str, NodeIndex)]) -> Self {
        for (value, index) in edges.iter() {
            self.add_normal_edge((*value).into(), *index);
        }
        self
    }

    pub fn get_matching_edges<'a>(
        &self,
        input: &'a str,
    ) -> Vec<(NodeIndex, &'a str, Option<WithIdentifier<EdgeData>>)> {
        let length = input.len();
        let normal_edges = self
            .normal_edges
            .iter()
            .filter(|(len, _)| len <= &&length)
            .flat_map(|(len, hm)| {
                if let Some(edges) = hm.get(&input[..*len]) {
                    let rest = input.split_at(*len).1;
                    edges
                        .into_iter()
                        .map(|index| (*index, rest, None))
                        .collect()
                } else {
                    vec![]
                }
            });
        let regex_edges = self
            .regex_edges
            .iter()
            .filter_map(|x| {
                if let Some(re_match) = x.expression.find(input) {
                    if re_match.start() > 0 {
                        None
                    } else {
                        let rest = &input[re_match.end()..];
                        let mut out = vec![];
                        for target_node in x.target.iter() {
                            out.push((
                                *target_node,
                                rest,
                                Some(WithIdentifier {
                                    identifier: x.identifier.clone(),
                                    payload: EdgeData::Match(re_match.as_str().into()),
                                }),
                            ));
                        }
                        Some(out)
                    }
                } else {
                    None
                }
            })
            .flatten();
        let mut substitution_edges = vec![];
        for edge in self.substitution_edges.iter() {
            for needle in edge.needles.iter() {
                // TODO: use num_matches
                if let Some((rest, _num_matches)) = subsequence(input, &needle.0) {
                    for target_node in edge.target.iter() {
                        substitution_edges.push((
                            *target_node,
                            rest,
                            Some(WithIdentifier {
                                identifier: edge.identifier.clone(),
                                payload: EdgeData::Substitution(needle.1.clone()),
                            }),
                        ));
                    }
                }
            }
        }
        normal_edges
            .chain(regex_edges)
            .chain(substitution_edges)
            .collect()
    }

    pub fn get_suggestions<'a>(&'a self) -> Vec<(&'a String, &'a Vec<NodeIndex>)> {
        let normal_edges = self.normal_edges.values().flat_map(|x| x.iter());
        let regex_edges = self.regex_edges.iter().map(|x| (&x.suggestion, &x.target));
        let substitution_edges = self
            .substitution_edges
            .iter()
            .map(|x| (&x.suggestion, &x.target));
        normal_edges
            .chain(regex_edges)
            .chain(substitution_edges)
            .collect()
    }
}

impl<T: Clone> Node<T> {
    fn shift(&self, shift: usize) -> Self {
        Node {
            payload: self.payload.clone(),
            is_final: self.is_final,
            normal_edges: self
                .normal_edges
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
            regex_edges: self
                .regex_edges
                .iter()
                .map(|x| RegExEdge {
                    expression: x.expression.clone(),
                    suggestion: x.suggestion.clone(),
                    target: x.target.iter().map(|x| x + shift).collect(),
                    identifier: x.identifier.clone(),
                })
                .collect(),
            substitution_edges: self
                .substitution_edges
                .iter()
                .map(|x| SubstitutionEdge {
                    needles: x.needles.clone(),
                    suggestion: x.suggestion.clone(),
                    target: x.target.iter().map(|x| x + shift).collect(),
                    identifier: x.identifier.clone(),
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
                ret.nodes[*index].add_normal_edge("".into(), root);
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
            nodes: vec![Node::default()],
            root: 0,
        };
        let mut shift = ret.nodes.len();
        for nfa in nfas.into_iter() {
            let root = shift + nfa.root;
            ret.nodes[0].add_normal_edge("".into(), root);
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
            Node::default().with_normal_edge(input.into(), 1),
            Node::default().with_payload_and_final(()),
        ];
        NFA { nodes, root: 0 }
    }

    pub fn match_non_empty_prefixes(input: &str) -> NFA<()> {
        let nodes = vec![
            Node::default().with_normal_edges(
                &((1..input.len() + 1)
                    .map(|len| (&input[0..len], 1))
                    .collect::<Vec<_>>()),
            ),
            Node::default().with_payload_and_final(()),
        ];
        NFA { nodes, root: 0 }
    }

    pub fn match_zero_or_more_spaces() -> NFA<()> {
        let nodes = vec![Node::default()
            .with_payload_and_final(())
            .with_normal_edge(" ".into(), 0)];
        NFA { nodes, root: 0 }
    }

    pub fn match_one_or_more_spaces() -> NFA<()> {
        let nodes = vec![
            Node::default().with_normal_edge(" ".into(), 1),
            Node::default()
                .with_payload_and_final(())
                .with_normal_edge(" ".into(), 1),
        ];
        NFA { nodes, root: 0 }
    }

    pub fn regex(identifier: String, regex: regex::Regex) -> NFA<()> {
        let nodes = vec![
            Node::default().with_regex_edge(RegExEdge {
                expression: regex,
                suggestion: "<QUERY>".into(), // TODO: set suggestions?
                target: vec![1],
                identifier,
            }),
            Node::default().with_payload_and_final(()),
        ];
        NFA { nodes, root: 0 }
    }

    pub fn substitution(
        identifier: String,
        needles: Vec<(String, HashMap<String, String>)>,
    ) -> NFA<()> {
        let nodes = vec![
            Node::default().with_substitution_edge(SubstitutionEdge {
                suggestion: format!("<{}>", identifier),
                identifier,
                target: vec![1],
                needles,
            }),
            Node::default().with_payload_and_final(()),
        ];
        NFA { nodes, root: 0 }
    }
}

impl<T> NFA<T> {
    pub fn nothing() -> NFA<T> {
        NFA {
            nodes: vec![Node::default()],
            root: 0,
        }
    }

    fn node_parse<'a, 'b>(
        &'a self,
        node_self: &Node<T>,
        input: &'b str,
    ) -> Vec<(&'a Node<T>, &'b str, Option<WithIdentifier<EdgeData>>)> {
        node_self
            .get_matching_edges(input)
            .into_iter()
            .map(|(index, rest, edge_payload)| (&self.nodes[index], rest, edge_payload))
            .collect()
    }
}

pub trait Parser<T> {
    fn parse<'a, 'b>(&'a self, input: &'b str) -> Vec<(&'a T, &'b str)>;
}

#[derive(Clone, Debug)]
pub struct Parsed<'a, T> {
    pub payload: &'a T,
    pub trace: Vec<Trace<&'a T>>,
}

#[derive(Clone, Debug)]
pub struct Suggestion<'a, T> {
    pub payload: &'a Option<T>,
    pub suggestion: String,
}

impl<T: std::fmt::Debug> NFA<T> {
    pub fn parse_full<'a>(&'a self, input: &str) -> Vec<&'a T> {
        self.parse(input)
            .into_iter()
            .filter_map(|x| if x.1.is_empty() { Some(x.0) } else { None })
            .collect()
    }

    pub fn parse_full_and_suggest<'a, 'b>(
        &'a self,
        input: &'b str,
    ) -> (Vec<Parsed<'a, T>>, Vec<Suggestion<'a, T>>) {
        // TODO: collect payloads
        let mut state = VecDeque::from([(&self.nodes[self.root], input, vec![])]);
        let mut output = vec![];
        let mut suggestion_states: VecDeque<(_, Vec<&str>)> = VecDeque::from([]);
        while let Some((node, string, mut payloads)) = state.pop_front() {
            if string.is_empty() {
                suggestion_states.push_back((node, vec![]))
            }
            if let Some(ref payload) = node.payload {
                payloads.push(Trace::Node(payload));
            }
            if node.is_final {
                if let Some(ref payload) = node.payload {
                    output.push((payload, string, payloads.clone()));
                } else {
                    // TODO: what is it's missing, or have value and not final...
                }
            }
            state.extend(self.node_parse(node, string).into_iter().map(
                |(node, rest, edge_payload)| {
                    let mut payloads = payloads.clone();
                    if let Some(edge_payload) = edge_payload {
                        payloads.push(Trace::Edge(edge_payload));
                    }
                    (node, rest, payloads)
                },
            ));
        }
        // BFS for finding suggestions
        // TODO: suggestions are ugly here, i.e. for shortcuts, we would like to have something
        // else for suggestions.
        let mut suggestions = vec![];
        let mut visited: HashSet<NodeIndex> = Default::default();
        let mut skip_suggesting_until = suggestion_states.len();
        while let Some((node, suggestion)) = suggestion_states.pop_front() {
            if node.is_final && skip_suggesting_until <= 0 {
                suggestions.push(Suggestion {
                    suggestion: format!("{}{}", input, suggestion.join("")),
                    payload: &node.payload,
                })
            }
            if skip_suggesting_until > 0 {
                skip_suggesting_until -= 1;
            }
            for (text, target_nodes) in node.get_suggestions() {
                for target_node in target_nodes.iter().filter(|x| visited.insert(**x)) {
                    let mut suggestion = suggestion.clone();
                    suggestion.push(text);
                    suggestion_states.push_back((&self.nodes[*target_node], suggestion))
                }
            }
        }

        (
            output
                .into_iter()
                .filter_map(|x| {
                    if x.1.is_empty() {
                        Some(Parsed {
                            payload: x.0,
                            trace: x.2,
                        })
                    } else {
                        None
                    }
                })
                .collect(),
            suggestions,
        )
    }
}

impl<T: std::fmt::Debug> Parser<T> for NFA<T> {
    fn parse<'a, 'b>(&'a self, input: &'b str) -> Vec<(&'a T, &'b str)> {
        // TODO: collect payloads
        let mut state = VecDeque::from([(&self.nodes[self.root], input)]);
        let mut output = vec![];
        while let Some((node, string)) = state.pop_front() {
            if node.is_final {
                if let Some(ref payload) = node.payload {
                    output.push((payload, string));
                } else {
                    // TODO: what is it's missing, or have value and not final...
                }
            }
            state.extend(
                self.node_parse(node, string)
                    .into_iter()
                    .map(|(a, b, _c)| (a, b)),
            );
        }
        output
    }
}

fn subsequence<'a>(input: &'a str, query: &str) -> Option<(&'a str, usize)> {
    let mut input_pointer = 0;
    let mut query_pointer = 0;
    let mut matched = 0;
    let input_b = input.as_bytes();
    let query_b = query.as_bytes();
    while input_pointer < input_b.len() && query_pointer < query_b.len() {
        if input_b[input_pointer] != query_b[query_pointer] {
            query_pointer += 1;
            matched += 1;
        } else {
            input_pointer += 1;
            query_pointer += 1;
        }
    }
    if matched == 0 {
        None
    } else {
        while !input.is_char_boundary(input_pointer) && input_pointer > 0 {
            input_pointer -= 1
        }
        Some((&input[input_pointer..], matched))
    }
}

