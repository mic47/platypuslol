use std::collections::{HashMap, HashSet, VecDeque};

type Length = usize;
type NodeIndex = usize;

#[derive(Clone, Debug)]
pub struct RegExEdge {
    expression: regex::Regex,
    suggestion: String,
    target: Vec<NodeIndex>,
}

#[derive(Clone, Debug)]
pub struct Node<T> {
    pub payload: Option<T>,
    pub is_final: bool,
    normal_edges: HashMap<Length, HashMap<String, Vec<NodeIndex>>>,
    regex_edges: Vec<RegExEdge>,
}

impl<T> Node<T> {
    fn map<R, F: Fn(&T) -> R>(&self, f: &F) -> Node<R> {
        Node {
            payload: match self.payload {
                Some(ref p) => Some(f(p)),
                None => None,
            },
            is_final: self.is_final,
            normal_edges: self.normal_edges.clone(),
            regex_edges: self.regex_edges.clone(),
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

    pub fn get_matching_edges<'a>(&self, input: &'a str) -> Vec<(NodeIndex, &'a str)> {
        let length = input.len();
        let normal_edges = self
            .normal_edges
            .iter()
            .filter(|(len, _)| len <= &&length)
            .flat_map(|(len, hm)| {
                if let Some(edges) = hm.get(&input[..*len]) {
                    let rest = input.split_at(*len).1;
                    edges.into_iter().map(|index| (*index, rest)).collect()
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
                            out.push((*target_node, rest))
                        }
                        Some(out)
                    }
                } else {
                    None
                }
            })
            .flatten();
        normal_edges.chain(regex_edges).collect()
    }

    pub fn get_suggestions<'a>(&'a self) -> Vec<(&'a String, &'a Vec<NodeIndex>)> {
        let normal_edges = self.normal_edges.values().flat_map(|x| x.iter());
        let regex_edges = self.regex_edges.iter().map(|x| (&x.suggestion, &x.target));
        normal_edges.chain(regex_edges).collect()
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
            nodes: vec![Node {
                payload: None,
                is_final: false,
                normal_edges: HashMap::default(),
                regex_edges: Default::default(),
            }],
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
            Node {
                payload: None,
                is_final: false,
                normal_edges: HashMap::from([(
                    input.len(),
                    HashMap::from([(input.into(), vec![1])]),
                )]),
                regex_edges: Default::default(),
            },
            Node {
                payload: Some(()),
                is_final: true,
                normal_edges: HashMap::default(),
                regex_edges: Default::default(),
            },
        ];
        NFA { nodes, root: 0 }
    }

    pub fn match_non_empty_prefixes(input: &str) -> NFA<()> {
        let nodes = vec![
            Node {
                payload: None,
                is_final: false,
                normal_edges: (1..input.len() + 1)
                    .map(|len| (len, HashMap::from([(input[0..len].into(), vec![1])])))
                    .collect(),
                regex_edges: Default::default(),
            },
            Node {
                payload: Some(()),
                is_final: true,
                normal_edges: HashMap::default(),
                regex_edges: Default::default(),
            },
        ];
        NFA { nodes, root: 0 }
    }

    pub fn match_zero_or_more_spaces() -> NFA<()> {
        let nodes = vec![Node {
            payload: Some(()),
            is_final: true,
            normal_edges: HashMap::from([(1, HashMap::from([(" ".into(), vec![0])]))]),
            regex_edges: Default::default(),
        }];
        NFA { nodes, root: 0 }
    }

    pub fn match_one_or_more_spaces() -> NFA<()> {
        let nodes = vec![
            Node {
                payload: None,
                is_final: false,
                normal_edges: HashMap::from([(1, HashMap::from([(" ".into(), vec![1])]))]),
                regex_edges: Default::default(),
            },
            Node {
                payload: Some(()),
                is_final: true,
                normal_edges: HashMap::from([(1, HashMap::from([(" ".into(), vec![1])]))]),
                regex_edges: Default::default(),
            },
        ];
        NFA { nodes, root: 0 }
    }

    pub fn rest_of_string(x: String) -> NFA<()> {
        let nodes = vec![
            Node {
                payload: None,
                is_final: false,
                normal_edges: Default::default(),
                regex_edges: vec![RegExEdge {
                    expression: regex::Regex::new(r"\w+").unwrap(), // TODO: remove unwrap
                    suggestion: "<QUERY>".into(),
                    target: vec![1],
                }],
            },
            Node {
                payload: Some(()),
                is_final: true,
                normal_edges: Default::default(),
                regex_edges: Default::default(),
            },
        ];
        NFA { nodes, root: 0 }
    }
    pub fn word(x: String) -> NFA<()> {
        let nodes = vec![
            Node {
                payload: None,
                is_final: false,
                normal_edges: Default::default(),
                regex_edges: vec![RegExEdge {
                    expression: regex::Regex::new(r".+").unwrap(), // TODO: remove unwrap
                    suggestion: "<WORD>".into(),
                    target: vec![1],
                }],
            },
            Node {
                payload: Some(()),
                is_final: true,
                normal_edges: Default::default(),
                regex_edges: Default::default(),
            },
        ];
        NFA { nodes, root: 0 }
    }
}

impl<T> NFA<T> {
    pub fn nothing() -> NFA<T> {
        NFA {
            nodes: vec![Node {
                payload: None,
                is_final: false,
                normal_edges: HashMap::default(),
                regex_edges: Default::default(),
            }],
            root: 0,
        }
    }

    fn node_parse<'a, 'b>(
        &'a self,
        node_self: &Node<T>,
        input: &'b str,
    ) -> Vec<(&'a Node<T>, &'b str)> {
        node_self
            .get_matching_edges(input)
            .into_iter()
            .map(|(index, rest)| (&self.nodes[index], rest))
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

    // TODO: need 2 features
    // - [ ] Way to propagate captures in regex queries. Maybe something more general.
    // - [ ] Way to aggregate state / captures while searching.
    //       Probably something like edge can emit capture, and final node can get these. 
    //       So trace can be like
    //       NodePayload(xxx), EdgePayload(xxx), NodePayload(xxx), NodePayload(xxx).
    pub fn parse_full_and_suggest<'a, 'b>(
        &'a self,
        input: &'b str,
    ) -> (Vec<(&'a T, &'b str)>, Vec<(String, &'a Option<T>)>) {
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
            for (text, target_nodes) in node.get_suggestions() {
                for target_node in target_nodes.iter().filter(|x| visited.insert(**x)) {
                    let mut suggestion = suggestion.clone();
                    suggestion.push(text);
                    suggestion_states.push_back((&self.nodes[*target_node], suggestion))
                }
            }
        }

        (
            output.into_iter().filter(|x| x.1.is_empty()).collect(),
            suggestions,
        )
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
