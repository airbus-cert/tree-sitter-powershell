use std::{
    env,
    ffi::OsString,
    io::{self, BufRead, BufReader, Read, Write},
    process::{Child, Command, Stdio},
    sync::mpsc::{self, Receiver, RecvTimeoutError, Sender},
    thread,
    time::{Duration, Instant},
};

use serde::Serialize;

use crate::model::{Projection, Reply};

const VERSION: &str = "7.6.6";
const STARTUP_TIMEOUT: Duration = Duration::from_secs(15);
const REQUEST_TIMEOUT: Duration = Duration::from_secs(5);
const REAP_TIMEOUT: Duration = Duration::from_secs(2);
const MAX_REPLY_BYTES: u64 = 8 * 1024 * 1024;

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("oracle {operation}")]
    Io {
        operation: &'static str,
        #[source]
        source: io::Error,
    },
    #[error("encoding oracle request")]
    Encode(#[source] serde_json::Error),
    #[error("decoding oracle reply")]
    Decode(#[source] serde_json::Error),
    #[error("oracle reply exceeds {MAX_REPLY_BYTES} bytes")]
    OversizedReply,
    #[error("oracle closed its reply stream")]
    Eof,
    #[error("oracle transport disconnected for request {id}")]
    Disconnected { id: u64 },
    #[error("oracle deadline exceeded for request {id}")]
    Deadline { id: u64 },
    #[error("oracle reply {actual} does not match request {expected}")]
    Correlation { expected: u64, actual: u64 },
    #[error("unexpected oracle reply kind {kind} for request {id}")]
    ReplyKind { id: u64, kind: &'static str },
    #[error("oracle runtime {actual} does not match required PowerShell {VERSION}")]
    Version { actual: String },
    #[error("oracle failed request {id}, {message}")]
    Reported { id: u64, message: String },
    #[error("oracle request already in flight")]
    Busy,
    #[error("oracle request identifier exhausted")]
    IdExhausted,
    #[error("oracle did not exit after termination")]
    ReapDeadline,
}

#[derive(Serialize)]
struct Request<'a> {
    id: u64,
    source: &'a str,
}

pub struct Ticket {
    id: u64,
    deadline: Instant,
}

struct Process(Child);

impl Process {
    fn terminate(&mut self) -> Result<(), Error> {
        if self.0.try_wait().map_err(|source| Error::Io {
            operation: "checking exit",
            source,
        })?.is_some() {
            return Ok(());
        }
        if let Err(source) = self.0.kill() {
            if self.0.try_wait().ok().flatten().is_some() {
                return Ok(());
            }
            return Err(Error::Io { operation: "terminating", source });
        }
        let started = Instant::now();
        while started.elapsed() < REAP_TIMEOUT {
            if self.0.try_wait().map_err(|source| Error::Io {
                operation: "reaping",
                source,
            })?.is_some() {
                return Ok(());
            }
            thread::sleep(Duration::from_millis(1));
        }
        Err(Error::ReapDeadline)
    }
}

impl Drop for Process {
    fn drop(&mut self) {
        if let Err(error) = self.terminate() {
            let _ = writeln!(io::stderr().lock(), "oracle cleanup failed, {error}");
        }
    }
}

fn executable() -> OsString {
    if let Some(path) = env::var_os("POWERSHELL_FUZZ_PWSH") {
        return path;
    }
    if let Ok(path) = env::current_exe() {
        if let Some(directory) = path.parent() {
            let bundled = directory.join("powershell-oracle").join("pwsh");
            if bundled.is_file() {
                return bundled.into_os_string();
            }
        }
    }
    OsString::from("pwsh")
}

#[cfg(target_os = "linux")]
fn prepare_child(parent: u32) -> io::Result<()> {
    let signal = libc::c_ulong::try_from(libc::SIGKILL)
        .map_err(|_| io::Error::from_raw_os_error(libc::EINVAL))?;
    let zero = libc::c_ulong::from(0_u8);
    // SAFETY: This prctl operation takes scalar arguments and no memory pointers.
    if unsafe { libc::prctl(libc::PR_SET_PDEATHSIG, signal, zero, zero, zero) } == -1 {
        return Err(io::Error::last_os_error());
    }
    // SAFETY: getppid has no caller-side memory or synchronization requirements.
    let actual = u32::try_from(unsafe { libc::getppid() })
        .map_err(|_| io::Error::from_raw_os_error(libc::ESRCH))?;
    if actual != parent {
        return Err(io::Error::from_raw_os_error(libc::ESRCH));
    }
    Ok(())
}

#[cfg(target_os = "linux")]
fn bind_parent_lifetime(command: &mut Command) {
    use std::os::unix::process::CommandExt;
    let parent = std::process::id();
    // SAFETY: The callback uses only scalar conversions and async-signal-safe syscalls.
    unsafe { command.pre_exec(move || prepare_child(parent)); }
}

fn read_reply(reader: &mut impl BufRead) -> Result<Reply, Error> {
    let mut bytes = Vec::new();
    let count = reader.take(MAX_REPLY_BYTES + 1)
        .read_until(b'\n', &mut bytes)
        .map_err(|source| Error::Io { operation: "reading reply", source })?;
    if count == 0 {
        return Err(Error::Eof);
    }
    if u64::try_from(count).map_or(true, |count| count > MAX_REPLY_BYTES) {
        return Err(Error::OversizedReply);
    }
    serde_json::from_slice(&bytes).map_err(Error::Decode)
}

pub struct Oracle {
    process: Process,
    requests: Sender<Vec<u8>>,
    replies: Receiver<Result<Reply, Error>>,
    next_id: u64,
    pending: Option<u64>,
}

impl Oracle {
    pub fn new() -> Result<Self, Error> {
        let mut command = Command::new(executable());
        command.args(["-NoLogo", "-NoProfile", "-NonInteractive", "-Command", include_str!("../oracle.ps1")])
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::inherit());
        #[cfg(target_os = "linux")]
        bind_parent_lifetime(&mut command);
        let started = Instant::now();
        let mut process = Process(command.spawn().map_err(|source| Error::Io {
            operation: "starting pinned runtime",
            source,
        })?);
        let mut input = process.0.stdin.take().ok_or_else(|| Error::Io {
            operation: "opening input pipe",
            source: io::Error::from(io::ErrorKind::BrokenPipe),
        })?;
        let output = process.0.stdout.take().ok_or_else(|| Error::Io {
            operation: "opening output pipe",
            source: io::Error::from(io::ErrorKind::BrokenPipe),
        })?;
        let (requests, jobs) = mpsc::channel::<Vec<u8>>();
        let (responses, replies) = mpsc::channel();
        thread::Builder::new().name("powershell-oracle".into()).spawn(move || {
            let mut output = BufReader::new(output);
            let ready = read_reply(&mut output);
            let failed = ready.is_err();
            if responses.send(ready).is_err() || failed {
                return;
            }
            for request in jobs {
                let response = input.write_all(&request)
                    .and_then(|()| input.flush())
                    .map_err(|source| Error::Io { operation: "writing request", source })
                    .and_then(|()| read_reply(&mut output));
                let failed = response.is_err();
                if responses.send(response).is_err() || failed {
                    return;
                }
            }
        }).map_err(|source| Error::Io { operation: "starting transport", source })?;
        let oracle = Self { process, requests, replies, next_id: 1, pending: None };
        match oracle.receive(0, STARTUP_TIMEOUT.saturating_sub(started.elapsed()))? {
            Reply::Ready { id, version, framework } => {
                if id != 0 {
                    return Err(Error::Correlation { expected: 0, actual: id });
                }
                if version != VERSION {
                    return Err(Error::Version { actual: version });
                }
                eprintln!("PowerShell differential oracle {version} ({framework})");
            }
            Reply::Error { id, message } => return Err(Error::Reported { id, message }),
            Reply::Parsed { id, .. } => return Err(Error::ReplyKind { id, kind: "parsed" }),
        }
        Ok(oracle)
    }

    fn receive(&self, id: u64, remaining: Duration) -> Result<Reply, Error> {
        self.replies.recv_timeout(remaining).map_err(|error| match error {
            RecvTimeoutError::Timeout => Error::Deadline { id },
            RecvTimeoutError::Disconnected => Error::Disconnected { id },
        })?
    }

    pub fn begin(&mut self, source: &str) -> Result<Ticket, Error> {
        if self.pending.is_some() {
            return Err(Error::Busy);
        }
        let id = self.next_id;
        self.next_id = id.checked_add(1).ok_or(Error::IdExhausted)?;
        let deadline = Instant::now() + REQUEST_TIMEOUT;
        let mut request = serde_json::to_vec(&Request { id, source }).map_err(Error::Encode)?;
        request.push(b'\n');
        self.requests.send(request).map_err(|_| Error::Disconnected { id })?;
        self.pending = Some(id);
        Ok(Ticket { id, deadline })
    }

    pub fn finish(&mut self, ticket: Ticket) -> Result<Projection, Error> {
        if self.pending != Some(ticket.id) {
            return Err(Error::Correlation {
                expected: self.pending.unwrap_or(0),
                actual: ticket.id,
            });
        }
        let reply = self.receive(ticket.id, ticket.deadline.saturating_duration_since(Instant::now()))?;
        match reply {
            Reply::Parsed { id, projection } => {
                if id != ticket.id {
                    return Err(Error::Correlation { expected: ticket.id, actual: id });
                }
                self.pending = None;
                Ok(projection)
            }
            Reply::Error { id, message } => {
                if id != ticket.id {
                    return Err(Error::Correlation { expected: ticket.id, actual: id });
                }
                Err(Error::Reported { id, message })
            }
            Reply::Ready { id, .. } => Err(Error::ReplyKind { id, kind: "ready" }),
        }
    }

    pub fn terminate(&mut self) -> Result<(), Error> {
        self.process.terminate()
    }
}

#[cfg(test)]
mod tests {
    use super::Oracle;
    use crate::model::Role;

    #[test]
    fn unicode_and_colon_value_spans_use_utf8_bytes() {
        let mut oracle = Oracle::new().unwrap();
        let source = "echo -Name:pré${x}🙂";
        let ticket = oracle.begin(source).unwrap();
        let projection = oracle.finish(ticket).unwrap();
        assert!(projection.errors.is_empty());
        assert_eq!(projection.commands.len(), 1);
        assert_eq!(projection.commands[0].name, [0, 4]);
        assert_eq!(projection.commands[0].arguments, [crate::model::Argument { span: [5, source.len()], collapsed: false }]);
        let variable = projection.semantic.iter().find(|item| item.role == Role::Variable).unwrap();
        let start = source.find("${x}").unwrap();
        assert_eq!((variable.start, variable.end), (start, start + 4));
    }

    #[test]
    fn invalid_namespace_is_diagnostic_and_next_request_is_independent() {
        let mut oracle = Oracle::new().unwrap();
        let ticket = oracle.begin("echo $:::name").unwrap();
        let malformed = oracle.finish(ticket).unwrap();
        assert!(malformed.errors.iter().any(|error| error.id == "InvalidVariableReference"));
        let ticket = oracle.begin("echo $::").unwrap();
        let valid = oracle.finish(ticket).unwrap();
        assert!(valid.errors.is_empty());
        let variable = valid.semantic.iter().find(|item| item.role == Role::Variable).unwrap();
        assert_eq!((variable.start, variable.end), (5, 8));
    }

    #[test]
    fn eof_diagnostic_offset_is_clamped_not_dropped() {
        let mut oracle = Oracle::new().unwrap();
        let source = "cargo publish --features=$(";
        let ticket = oracle.begin(source).unwrap();
        let projection = oracle.finish(ticket).unwrap();
        assert!(projection.errors.iter().any(|error| {
            error.id == "MissingEndParenthesisInSubexpression" && error.start == source.len() && error.end == source.len()
        }));
    }
}
