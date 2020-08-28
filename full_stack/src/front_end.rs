use std::collections::VecDeque;
use std::fmt::{self, Display};
use std::io;

/// Represents streams of bytes that can be interpreted as Front End code.
pub trait CodeStream {
    type Error;
    fn next(&mut self) -> Result<Option<u8>, Self::Error>;
    fn peek(&self) -> Result<Option<u8>, Self::Error>;
}

/// Represents an error encountered while running a program.
pub enum Error<S: CodeStream> {
    /// We got EOF without the block being closed.
    UnexpectedEof,
    /// We got an invalid byte.
    Unexpected(u8),
    /// The repetition count was too large.
    RepOverflow(String),
    /// An error from the input stream.
    Stream(S::Error),
    /// An error encountered while doing IO.
    Io(io::Error),
}

impl<S: CodeStream> Display for Error<S>
    where S::Error: Display
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::UnexpectedEof =>
                f.write_str("unexpected end of file"),
            Self::Unexpected(b) =>
                write!(f, "unexpected instruction: '{}'",
                    std::ascii::escape_default(*b)),
            Self::RepOverflow(s) =>
                write!(f, "repetition suffix is too large: {}", s),
            Self::Stream(e) => e.fmt(f),
            Self::Io(e) => e.fmt(f),
        }
    }
}

/// Read a character from the input stream and advance the position.
fn next<S: CodeStream>(stream: &mut S) -> Result<u8, Error<S>> {
    stream.next().map_err(Error::Stream)?.ok_or(Error::UnexpectedEof)
}

/// Read a character from the input stream without advancing.
fn peek<S: CodeStream>(stream: &S) -> Result<Option<u8>, Error<S>> {
    stream.peek().map_err(Error::Stream)
}

/// Read and discard whitespace characters from the input stream.
fn discard_space<S: CodeStream>(stream: &mut S)
    -> Result<(), Error<S>>
{
    while peek(stream)?.map_or(false, |b| b.is_ascii_whitespace()) {
        next(stream)?;
    }
    Ok(())
}

/// Attempt to decode a number from the input stream.
fn decode_num<S: CodeStream>(stream: &mut S)
    -> Result<u32, Error<S>>
{
    discard_space(stream)?;
    let mut digits = String::new();
    while let Some(d) = peek(stream)?.filter(u8::is_ascii_digit) {
        digits.push(d as char);
        next(stream)?;
    }
    if digits.is_empty() {
        Ok(1)
    } else {
        digits.parse::<u32>().map_err(|e| {
            // the only expected kind of error is an overflow error
            assert_eq!(e.to_string(),
                "number too large to fit in target type");
            Error::RepOverflow(digits)
        })
    }
}

/// Decode a block literal from the input stream and return its
/// contents. Stop once a closing `}` is found.
fn decode_lit<S: CodeStream>(stream: &mut S)
    -> Result<Vec<u8>, Error<S>>
{
    let mut content = Vec::new();
    let mut nest = 0u32;
    loop {
        let b = next(stream)?;
        if b == b'{' {
            nest += 1;
        } else if b == b'}' {
            if nest == 0 {
                break;
            }
            nest -= 1;
        }
        content.push(b);
    }
    Ok(content)
}

/// Represents a Front End instruction.
pub enum Instr {
    Add(u8),
    Sub(u8),
    Dup(u32),
    Del(u32),
    Swap,
    Get(u8),
    Set(u8),
    Lit(Vec<u8>),
    Block {
        // `[` or `(`?
        zero: bool,
        // `]` or `)`?
        repeat: bool,
        // Inner content
        content: Vec<Instr>,
    }
}

/// Decode an instruction from the input stream. If `top_level`
/// is true, return `None` when the closing bracket is encountered.
pub fn decode_instr<S: CodeStream>(stream: &mut S, top_level: bool)
   -> Result<Option<Instr>, Error<S>>
{
    discard_space(stream)?;
    Ok(match next(stream)? {
        // arithmetic is 8-bit and wrapping, so the truncation
        // is correct behavior
        b'+' => Some(Instr::Add(decode_num(stream)? as u8)),
        b'-' => Some(Instr::Sub(decode_num(stream)? as u8)),
        b':' => Some(Instr::Dup(decode_num(stream)?)),
        b'!' => Some(Instr::Del(decode_num(stream)?)),
        b'/' => Some(Instr::Swap),
        b @ b'[' | b @ b'(' => {
            let zero = b == b'[';
            let mut content = Vec::new();
            loop {
                match peek(stream)? {
                    Some(b @ b']') | Some(b @ b')') => {
                        next(stream)?;
                        let repeat = b == b']';
                        break Some(Instr::Block { zero, repeat, content })
                    }
                    Some(_) => content.push(
                        decode_instr(stream, false)?
                            .expect("> shouldn't be `None` in this context")),
                    None => return Err(Error::UnexpectedEof),
                }
            }
        }
        b'{' => Some(Instr::Lit(decode_lit(stream)?)),
        b'>' if top_level => None,
        b => if b.is_ascii_lowercase() {
            Some(Instr::Get(b - b'a'))
        } else if b.is_ascii_uppercase() {
            Some(Instr::Set(b - b'A'))
        } else {
            return Err(Error::Unexpected(b))
        }
    })
}

impl CodeStream for VecDeque<u8> {
    type Error = std::convert::Infallible;
    fn next(&mut self) -> Result<Option<u8>, Self::Error> {
        Ok(self.pop_front())
    }
    fn peek(&self) -> Result<Option<u8>, Self::Error> {
        Ok(self.get(0).copied())
    }
}
