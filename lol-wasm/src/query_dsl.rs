use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum DslWord {
    Prefix(String),
    Optional(String),
    QueryString(String),
    Query(String),
    SubstitutionQuery(Substitution),
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Substitution {
    pub name: String,
    pub field: String,
}

impl DslWord {
    pub fn parse(input: &str) -> Self {
        match input.chars().nth(0) {
            None => DslWord::Prefix(input.into()),
            Some(head) => match head {
                '{' => DslWord::QueryString(input.into()),
                '[' => DslWord::Optional(input.into()),
                '<' => DslWord::Query(input.into()),
                // Just parse into 
                '!' => DslWord::SubstitutionQuery(todo!()),
                _ => DslWord::Prefix(input.into()),
            },
        }
    }

    pub fn parse_sentence(input: &str) -> Vec<Self> {
        input.split(" ")
            .filter(|x| !x.is_empty())
            .map(Self::parse).collect()
    }
}
