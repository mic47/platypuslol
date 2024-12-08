use std::{
    cmp::Ordering,
    collections::{HashMap, HashSet, VecDeque},
    rc::Rc,
};

use itertools::Itertools;

use crate::Regex;

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
    expression: Regex,
    suggestion: String,
    target: Vec<NodeIndex>,
    identifier: String,
}

#[derive(Clone, Debug)]
pub struct SubstitutionEdge {
    needles: Vec<(String, HashMap<String, String>)>,
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

impl<T: std::fmt::Debug> Node<T> {
    pub fn with_payload_for_final<R: Clone>(self, payload: &R) -> Node<R> {
        Node {
            payload: if self.is_final {
                Some(payload.clone())
            } else {
                None
            },
            is_final: self.is_final,
            normal_edges: self.normal_edges,
            regex_edges: self.regex_edges,
            substitution_edges: self.substitution_edges,
        }
    }

    pub fn make_final(mut self) -> Self {
        self.is_final = true;
        self
    }

    fn map<R, F: Fn(&T) -> R>(&self, f: &F) -> Node<R> {
        Node {
            payload: self.payload.as_ref().map(f),
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
    ) -> Vec<(NodeIndex, &'a str, f64, Option<WithIdentifier<EdgeData>>)> {
        let length = input.len();
        let normal_edges = self
            .normal_edges
            .iter()
            .filter(|(len, _)| len <= &&length)
            .flat_map(|(len, hm)| {
                if let Some(edges) = hm.get(&input[..*len]) {
                    let rest = input.split_at(*len).1;
                    edges.iter().map(|index| (*index, rest, 0., None)).collect()
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
                                0.,
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
                if let Some((rest, num_matches)) = subsequence(input, &needle.0) {
                    let needle_len = needle.0.len();
                    for target_node in edge.target.iter() {
                        substitution_edges.push((
                            *target_node,
                            rest,
                            if needle_len == 0 {
                                0.
                            } else {
                                (num_matches as f64 - needle_len as f64) / needle_len as f64
                            },
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

    #[allow(clippy::type_complexity)]
    pub fn get_suggestions(&self) -> Vec<(Vec<(&str, Option<Trace<&T>>)>, &Vec<NodeIndex>)> {
        let normal_edges = self
            .normal_edges
            .values()
            .flat_map(|x| x.iter())
            .map(|(a, b)| (vec![(a.as_str(), None)], b));
        let regex_edges = self
            .regex_edges
            .iter()
            .map(|x| (vec![(x.suggestion.as_str(), None)], &x.target)); // TODO?
        let substitution_edges = self.substitution_edges.iter().map(|x| {
            (
                x.needles
                    .iter()
                    .map(|(needle, subst)| {
                        (
                            needle.as_str(),
                            Some(Trace::Edge(WithIdentifier {
                                identifier: x.identifier.clone(),
                                payload: EdgeData::Substitution(subst.clone()),
                            })),
                        )
                    })
                    .collect(),
                &x.target,
            )
        });
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

impl<T: std::fmt::Debug> NFA<T> {
    pub fn with_payload_for_final_nodes<R: Clone>(self, payload: &R) -> NFA<R> {
        NFA {
            nodes: self
                .nodes
                .into_iter()
                .map(|node| node.with_payload_for_final(payload))
                .collect(),
            root: self.root,
        }
    }

    pub fn map<R, F: Fn(&T) -> R>(&self, f: &F) -> NFA<R> {
        NFA {
            nodes: self.nodes.iter().map(|x| x.map(f)).collect(),
            root: self.root,
        }
    }
}

impl<T: Clone + std::fmt::Debug> NFA<T> {
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
        for nfa in nfas[1..].iter() {
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
        for nfa in nfas.iter() {
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
            Node::default().make_final(),
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
            Node::default().make_final(),
        ];
        NFA { nodes, root: 0 }
    }

    pub fn match_zero_or_more_spaces() -> NFA<()> {
        let nodes = vec![Node::default().make_final().with_normal_edge(" ".into(), 0)];
        NFA { nodes, root: 0 }
    }

    pub fn match_one_or_more_spaces() -> NFA<()> {
        let nodes = vec![
            Node::default().with_normal_edge(" ".into(), 1),
            Node::default().make_final().with_normal_edge(" ".into(), 1),
        ];
        NFA { nodes, root: 0 }
    }

    pub fn regex(identifier: String, regex: Regex) -> NFA<()> {
        let nodes = vec![
            Node::default().with_regex_edge(RegExEdge {
                expression: regex,
                suggestion: "<QUERY>".into(), // TODO: set suggestions?
                target: vec![1],
                identifier,
            }),
            Node::default().make_final(),
        ];
        NFA { nodes, root: 0 }
    }

    pub fn substitution(
        identifier: String,
        needles: Vec<(String, HashMap<String, String>)>,
    ) -> NFA<()> {
        let nodes = vec![
            Node::default().with_substitution_edge(SubstitutionEdge {
                identifier,
                target: vec![1],
                needles,
            }),
            Node::default().make_final(),
        ];
        NFA { nodes, root: 0 }
    }
}

type ParserOutputTuple<'a, 'b, T> = (&'a Node<T>, &'b str, f64, Option<WithIdentifier<EdgeData>>);

impl<T: std::fmt::Debug> NFA<T> {
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
    ) -> Vec<ParserOutputTuple<'a, 'b, T>> {
        node_self
            .get_matching_edges(input)
            .into_iter()
            .map(|(index, rest, score, edge_payload)| {
                (&self.nodes[index], rest, score, edge_payload)
            })
            .collect()
    }
}

#[derive(Clone, Debug)]
pub struct Parsed<'a, T> {
    pub payload: &'a T,
    pub score: f64,
    pub trace: List<Trace<&'a T>>,
}

#[derive(Clone, Debug)]
pub struct Suggestion<'a, T> {
    pub payload: &'a Option<T>,
    pub suggestion: String,
    pub trace: Vec<Trace<&'a T>>,
}

#[allow(clippy::type_complexity)]
#[derive(Clone, Debug)]
struct BFSSuggestions<'a, T> {
    pub node: &'a Node<T>,
    pub suggestions_and_traces: List<Vec<(&'a str, Option<Trace<&'a T>>)>>,
}

impl<'a, T> BFSSuggestions<'a, T> {
    pub fn get_suggestions(&self) -> Vec<Suggestion<'a, T>> {
        let suggestions_and_traces = self
            .suggestions_and_traces
            .clone_to_vec()
            .into_iter()
            .filter(|x| !x.is_empty())
            .collect::<Vec<_>>();
        if suggestions_and_traces.is_empty() {
            return vec![];
        }
        let mut output = vec![];
        let mut stack: Vec<usize> = vec![0];
        while let Some(top_index_position) = stack.last() {
            let current_index = stack.len() - 1;
            if *top_index_position >= suggestions_and_traces[current_index].len() {
                stack.pop();
                if !stack.is_empty() {
                    let last = stack.len() - 1;
                    stack[last] += 1;
                }
                continue;
            }
            while stack.len() < suggestions_and_traces.len() {
                stack.push(0);
            }
            output.push(Suggestion {
                payload: &self.node.payload,
                suggestion: stack
                    .iter()
                    .enumerate()
                    .map(|(index, value)| suggestions_and_traces[index][*value].0)
                    .join(" "),
                trace: stack
                    .iter()
                    .enumerate()
                    .filter_map(|(index, value)| suggestions_and_traces[index][*value].1.clone())
                    .collect(),
            });
            let last = stack.len() - 1;
            stack[last] += 1;
        }
        output
    }
}

#[derive(Debug)]
struct ListItem<T> {
    data: T,
    prev: Option<Rc<ListItem<T>>>,
}

#[derive(Clone, Debug)]
pub struct List<T> {
    list: Option<Rc<ListItem<T>>>,
}

impl<T> Default for List<T> {
    fn default() -> Self {
        Self { list: None }
    }
}

impl<T> List<T> {
    pub fn new(data: T) -> Self {
        Self {
            list: Some(Rc::new(ListItem { data, prev: None })),
        }
    }
    pub fn push(&mut self, data: T) {
        let new_list = Some(Rc::new(ListItem {
            data,
            prev: self.list.clone(),
        }));
        self.list = new_list;
    }
}
impl<T: Clone> List<T> {
    pub fn clone_to_vec(&self) -> Vec<T> {
        let mut out = vec![];
        let mut list = self.list.clone();
        while let Some(item) = list {
            out.push(item.data.clone());
            list = item.prev.clone();
        }
        out.reverse();
        out
    }
}

struct ParseState<'a, 'b, T> {
    node: &'a Node<T>,
    unparsed_string: &'b str,
    score: f64,
    payloads: List<Trace<&'a T>>,
}

impl<T: std::fmt::Debug> NFA<T> {
    pub fn parse_full_and_suggest<'a>(
        &'a self,
        input: &str,
    ) -> (Vec<Parsed<'a, T>>, Vec<Suggestion<'a, T>>) {
        // TODO: collect payloads
        let (output, suggestion_states) = self.parse_impl(input);

        let mut output: Vec<_> = output
            .into_iter()
            .filter_map(|x| {
                if x.1.is_empty() {
                    Some(Parsed {
                        payload: x.0,
                        score: x.2,
                        trace: x.3,
                    })
                } else {
                    None
                }
            })
            .collect();
        output.sort_by(|a, b| {
            a.score
                .partial_cmp(&b.score)
                .unwrap_or(Ordering::Greater)
                .reverse()
        });
        let suggestions = self.suggest_impl(suggestion_states);
        (output, suggestions)
    }

    fn suggest_impl<'a>(
        &'a self,
        mut suggestion_states: VecDeque<BFSSuggestions<'a, T>>,
    ) -> Vec<Suggestion<'a, T>> {
        // BFS for finding suggestions
        // TODO: suggestions are ugly here, i.e. for shortcuts, we would like to have something
        // else for suggestions.
        let mut suggestions = vec![];
        let mut visited: HashSet<NodeIndex> = Default::default();
        let mut skip_suggesting_until = suggestion_states.len();
        while let Some(suggestion_state) = suggestion_states.pop_front() {
            if suggestion_state.node.is_final && skip_suggesting_until == 0 {
                suggestions.extend(suggestion_state.get_suggestions());
            }
            skip_suggesting_until = skip_suggesting_until.saturating_sub(1);
            let mut to_visit: HashSet<NodeIndex> = Default::default();
            for (texts_with_traces, target_nodes) in suggestion_state.node.get_suggestions() {
                for target_node in target_nodes.iter().filter(|x| !visited.contains(*x)) {
                    to_visit.insert(*target_node);
                    let mut st = suggestion_state.suggestions_and_traces.clone();
                    st.push(texts_with_traces.clone());
                    suggestion_states.push_back(BFSSuggestions {
                        node: &self.nodes[*target_node],
                        suggestions_and_traces: st,
                    });
                }
            }
            visited.extend(to_visit);
        }
        suggestions
    }

    #[allow(clippy::type_complexity)]
    fn parse_impl<'a, 'b>(
        &'a self,
        input: &'b str,
    ) -> (
        Vec<(&'a T, &'b str, f64, List<Trace<&'a T>>)>,
        VecDeque<BFSSuggestions<'a, T>>,
    ) {
        let mut state = VecDeque::from([ParseState {
            node: &self.nodes[self.root],
            unparsed_string: input,
            score: 0.,
            payloads: List::default(),
        }]);
        let mut output = vec![];
        let mut suggestion_states: VecDeque<BFSSuggestions<T>> = VecDeque::from([]);
        while let Some(ParseState {
            node,
            unparsed_string,
            score,
            mut payloads,
        }) = state.pop_front()
        {
            if unparsed_string.is_empty() {
                suggestion_states.push_back(BFSSuggestions {
                    node,
                    // TODO Fix this
                    suggestions_and_traces: List::new(
                        payloads
                            .clone_to_vec()
                            .into_iter()
                            .map(|p| ("", Some(p)))
                            .collect(),
                    ),
                })
            }
            if let Some(ref payload) = node.payload {
                payloads.push(Trace::Node(payload));
            }
            if node.is_final {
                if let Some(ref payload) = node.payload {
                    output.push((payload, unparsed_string, score, payloads.clone()));
                } else {
                    // TODO: what is it's missing, or have value and not final...
                }
            }
            state.extend(self.node_parse(node, unparsed_string).into_iter().map(
                |(node, rest, edge_score, edge_payload)| {
                    let mut payloads = payloads.clone();
                    if let Some(edge_payload) = edge_payload {
                        payloads.push(Trace::Edge(edge_payload));
                    }
                    ParseState {
                        node,
                        unparsed_string: rest,
                        score: score + edge_score,
                        payloads,
                    }
                },
            ));
        }
        (output, suggestion_states)
    }

    #[cfg(test)]
    pub fn parse_for_tests<'a, 'b>(&'a self, input: &'b str) -> Vec<(&'a T, &'b str)> {
        self.parse_impl(input)
            .0
            .into_iter()
            .map(|x| (x.0, x.1))
            .collect()
    }

    #[cfg(test)]
    pub fn parse_full_for_tests<'a>(&'a self, input: &str) -> Vec<&'a T> {
        self.parse_full_and_suggest(input)
            .0
            .into_iter()
            .map(|x| x.payload)
            .collect()
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
        } else {
            input_pointer += 1;
            query_pointer += 1;
            matched += 1;
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
