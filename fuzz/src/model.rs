use serde::{Deserialize, Serialize};

pub type Span = [usize; 2];

#[derive(Debug, Deserialize, Serialize, PartialEq, Eq)]
pub struct Projection {
    pub errors: Vec<Diagnostic>,
    pub commands: Vec<Command>,
    pub semantic: Vec<Semantic>,
}

#[derive(Debug, Deserialize, Serialize, PartialEq, Eq)]
pub struct Diagnostic {
    pub id: String,
    pub start: usize,
    pub end: usize,
    pub incomplete: bool,
}

#[derive(Debug, Deserialize, Serialize, PartialEq, Eq)]
pub struct Command {
    pub start: usize,
    pub end: usize,
    pub name: Span,
    pub arguments: Vec<Argument>,
    pub redirections: Vec<Span>,
}

#[derive(Debug, Deserialize, Serialize, PartialEq, Eq)]
pub struct Argument {
    pub span: Span,
    /// Set only for an owner that may cover more than one oracle `CommandElement`,
    /// like `stop_parsing`'s single token owning both marker and tail.
    #[serde(default)]
    pub collapsed: bool,
}

#[derive(Debug, Deserialize, Serialize, PartialEq, Eq, PartialOrd, Ord)]
pub struct Semantic {
    pub role: Role,
    pub start: usize,
    pub end: usize,
}

#[derive(Clone, Copy, Debug, Deserialize, Serialize, PartialEq, Eq, PartialOrd, Ord)]
#[serde(rename_all = "snake_case")]
pub enum Role {
    Variable,
    Subexpression,
    Method,
    Member,
    Parentheses,
    ScriptBlock,
    Assignment,
}

#[derive(Debug, Deserialize)]
#[serde(tag = "type", rename_all = "snake_case")]
pub enum Reply {
    Ready {
        id: u64,
        version: String,
        framework: String,
    },
    Parsed {
        id: u64,
        projection: Projection,
    },
    Error {
        id: u64,
        message: String,
    },
}
