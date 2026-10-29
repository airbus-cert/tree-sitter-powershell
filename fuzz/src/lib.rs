#![doc = include_str!("../README.md")]

mod model;
mod oracle;
mod projection;

use std::{
    ops::ControlFlow,
    sync::LazyLock,
    time::{Duration, Instant},
};

use libfuzzer_sys::Corpus;
use model::Projection;
use parking_lot::Mutex;
use tree_sitter::{ParseOptions, Parser};

const MAX_SOURCE_BYTES: usize = 8192; // keep in sync with oracle.ps1's limit and powershell_differential.options' max_len
const PARSE_TIMEOUT: Duration = Duration::from_secs(2);

#[derive(Debug, thiserror::Error)]
enum Error {
    #[error("loading PowerShell grammar")]
    Language(#[from] tree_sitter::LanguageError),
    #[error(transparent)]
    Oracle(#[from] oracle::Error),
    #[error("tree-sitter exceeded its parsing deadline")]
    ParseDeadline,
    #[error("acceptance differs\nreference {reference:?}\nparser {parser:?}")]
    Acceptance { reference: Box<Projection>, parser: Box<Projection> },
    #[error("semantic projection differs\nreference {reference:?}\nparser {parser:?}")]
    Semantic { reference: Box<Projection>, parser: Box<Projection> },
}

struct Fuzzer {
    parser: Parser,
    oracle: oracle::Oracle,
}

impl Fuzzer {
    fn new() -> Result<Self, Error> {
        let mut parser = Parser::new();
        parser.set_language(&tree_sitter_powershell::LANGUAGE.into())?;
        Ok(Self { parser, oracle: oracle::Oracle::new()? })
    }

    fn check(&mut self, source: &str) -> Result<(), Error> {
        let ticket = self.oracle.begin(source)?;
        let deadline = Instant::now() + PARSE_TIMEOUT;
        let mut progress = |_: &tree_sitter::ParseState| {
            if Instant::now() >= deadline { ControlFlow::Break(()) } else { ControlFlow::Continue(()) }
        };
        let tree = self.parser.parse_with_options(
            &mut |offset, _| &source.as_bytes()[offset..],
            None,
            Some(ParseOptions::new().progress_callback(&mut progress)),
        ).ok_or(Error::ParseDeadline)?;
        if Instant::now() >= deadline {
            return Err(Error::ParseDeadline);
        }
        let mut reference = self.oracle.finish(ticket)?;
        let parser_rejects = tree.root_node().has_error();
        if reference.errors.is_empty() == parser_rejects {
            return Err(Error::Acceptance { reference: Box::new(reference), parser: Box::new(projection::project(&tree)) });
        }
        if parser_rejects {
            return Ok(());
        }
        let mut parser = projection::project(&tree);
        projection::normalize(&mut reference);
        projection::normalize(&mut parser);
        if !projection::commands_agree(&reference.commands, &parser.commands) || reference.semantic != parser.semantic {
            return Err(Error::Semantic { reference: Box::new(reference), parser: Box::new(parser) });
        }
        Ok(())
    }
}

/// Compares bounded UTF-8 source with the pinned parse-only PowerShell oracle.
///
/// # Panics
/// Panics on oracle failures, parser deadlines, or differential mismatches.
pub fn fuzz(input: &[u8]) -> Corpus {
    if input.len() > MAX_SOURCE_BYTES {
        return Corpus::Reject;
    }
    // libFuzzer always runs b"" once; main rejects it as ERROR while the oracle accepts it cleanly, a real defect left for a separate fix.
    if input.is_empty() {
        return Corpus::Reject;
    }
    let Ok(source) = std::str::from_utf8(input) else { return Corpus::Reject; };
    static STATE: LazyLock<Mutex<Fuzzer>> = LazyLock::new(|| {
        Mutex::new(Fuzzer::new().unwrap_or_else(|error| panic!("differential infrastructure failure, {error}")))
    });
    let mut state = STATE.lock();
    if let Err(error) = state.check(source) {
        let cleanup = state.oracle.terminate();
        drop(state);
        if let Err(cleanup) = cleanup {
            panic!("differential failure for {source:?}\n{error}\noracle cleanup failed, {cleanup}");
        }
        panic!("differential failure for {source:?}\n{error}");
    }
    Corpus::Keep
}
