use num_bigint::BigInt;

use std::fmt;

use crate::bytecode::{Bytecode, Opcode};
use crate::value::{Array, Block, Value};

/// An error encountered while parsing.
pub enum Error {
    Unexpected {
        ex: Expect,
        got: Outcome,
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Unexpected { ex, got } =>
                write!(f, "expected {}, got {}", ex, got),
        }
    }
}

/// A description of a particular thing the parser could be expecting.
#[derive(Clone, Copy)]
pub enum Expect {
    Instr,
    StrLiteral,
    CharLiteral,
}

impl fmt::Display for Expect {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Instr => write!(f, "instruction"),
            Self::StrLiteral => write!(f, "UTF-8 string literal"),
            Self::CharLiteral => write!(f, "UTF-8 character"),
        }
    }
}

/// A description of a particular malformed construct that the parser
/// could encounter.
#[derive(Clone, Copy)]
pub enum Outcome {
    Byte(u8),
    BadUtf8([u8; 4]),
    Eof,
}

impl fmt::Display for Outcome {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use std::ascii::escape_default;
        match self {
            Self::Byte(b) =>
                write!(f, "invalid byte '{}'", escape_default(*b)),
            Self::BadUtf8(bs) => {
                write!(f, "invalid UTF-8 sequence \"")?;
                for &b in bs {
                    if b.is_ascii() { break }
                    write!(f, "{}", escape_default(b))?;
                }
                write!(f, "\"")
            }
            Self::Eof =>
                write!(f, "end of file")
        }
    }
}

struct ParseState<'a> {
    code: &'a [u8],
    pos: usize,
    bytecode: Bytecode,
}

impl ParseState<'_> {
    /// Are there still bytes left to consume?
    fn not_eof(&mut self) -> bool {
        self.pos < self.code.len()
    }

    /// Return the next byte in the input.
    fn peek_byte(&mut self) -> Option<u8> {
        self.code.get(self.pos).copied()
    }

    /// Return the next byte in the input and advance the cursor past it.
    fn next_byte(&mut self) -> Option<u8> {
        let res = self.peek_byte()?;
        self.pos += 1;
        Some(res)
    }

    /// Decode a UTF-8 character from the input.
    fn next_char(&mut self, ex: Expect) -> Result<char, Error> {
        let mut buf = [0; 4];
        let mut i = 0;
        loop {
            buf[i] = self.next_byte().ok_or(Error::Unexpected {
                ex,
                got: Outcome::Eof
            })?;
            i += 1;
            if let Ok(s) = std::str::from_utf8(&buf[0..i]) {
                return Ok(s.chars().next().unwrap());
            } else if i == 4 {
                return Err(Error::Unexpected {
                    ex,
                    got: Outcome::BadUtf8(buf),
                });
            }
        }
    }

    /// Decode the contents of a string literal (without the leading
    /// quote).
    fn next_str(&mut self) -> Result<Value, Error> {
        let mut chars = Array::new();
        let mut escaped = false;
        loop {
            let c = self.next_char(Expect::StrLiteral)?;
            if escaped {
                if c != '\\' && c != '"' {
                    // if we find a backslash before non-escapable
                    // characters, just treat it literally
                    chars.push_back('\\'.into());
                }
                chars.push_back(c.into());
                escaped = false;
            } else if c == '\\' {
                escaped = true;
            } else if c == '"' {
                break;
            } else {
                chars.push_back(c.into());
            }
        }
        Ok(chars.into())
    }

    /// Decode an integer literal given its first digit.
    fn next_num(&mut self, first: u8) -> Value {
        let mut chars = vec![first];
        let mut is_decimal = first == b'.';
        loop {
            match self.peek_byte() {
                Some(b) if b.is_ascii_digit() => chars.push(b),
                Some(b'.') if !is_decimal => {
                    is_decimal = true;
                    chars.push(b'.');
                }
                _ => break,
            }
            let _ = self.next_byte();
        }
        // this function only adds ASCII values to the vector, so this
        // can only fail if the caller passed in a non-ASCII byte
        let chars = std::str::from_utf8(&chars).unwrap();
        if is_decimal {
            // potential bad values of `chars` that can cause a panic here:
            // "." and ".-"
            Value::Real(chars.parse::<f64>().unwrap())
        } else {
            // potential bad value of `chars` that can cause a panic here:
            // "-"
            Value::Int(chars.parse::<BigInt>().unwrap())
        }
    }

    /// Add an opcode to the bytecode.
    fn add_opcode(&mut self, op: Opcode) {
        self.bytecode.bytes.push(op as u8);
    }

    /// Add a literal value to the bytecode.
    fn add_lit(&mut self, val: Value) {
        self.bytecode.consts.push(val);
        self.add_opcode(Opcode::Lit);
    }

    /// Decode the next instruction.
    fn add_ins(&mut self) -> Result<(), Error> {
        use Opcode::*;
        let byte = match self.next_byte() {
            Some(b) => b,
            None => return Err(Error::Unexpected {
                ex: Expect::Instr,
                got: Outcome::Eof,
            })
        };
        match byte {
            b'\t' | b'\n' | b'\r' | b' ' => {}
            b'!' => self.add_opcode(Excl),
            b'"' => {
                let val = self.next_str()?;
                self.add_lit(val);
            }
            b'#' => self.add_opcode(Hash),
            b'$' => self.add_opcode(Dollar),
            b'%' => self.add_opcode(Percent),
            b'&' => self.add_opcode(And),
            b'\'' => {
                let c = self.next_char(Expect::CharLiteral)?;
                self.add_lit(c.into());
            }
            b'(' => self.add_opcode(LeftParen),
            b')' => self.add_opcode(RightParen),
            b'*' => self.add_opcode(Star),
            b'+' => self.add_opcode(Plus),
            b',' => self.add_opcode(Comma),
            b if b.is_ascii_digit() => {
                let val = self.next_num(b);
                self.add_lit(val);
            }
            b'[' => self.add_opcode(LeftBracket),
            b']' => self.add_opcode(RightBracket),
            b'_' => self.add_opcode(Underscore),
            b'a' => self.add_opcode(LowerA),
            b'{' => {
                let mut sub_state = ParseState {
                    code: self.code,
                    pos: self.pos,
                    bytecode: Bytecode::new(),
                };
                while sub_state.peek_byte() != Some(b'}') {
                    sub_state.add_ins()?;
                }
                self.pos = sub_state.pos + 1;
                self.add_lit(Value::Block(Block::new(sub_state.bytecode)));
            }
            b => return Err(Error::Unexpected {
                ex: Expect::Instr,
                got: Outcome::Byte(b),
            }),
        }
        Ok(())
    }
}

/// Parse the given program into bytecode form.
pub fn parse(code: &[u8]) -> Result<Bytecode, Error> {
    let mut state = ParseState {
        code,
        pos: 0,
        bytecode: Bytecode::new(),
    };
    while state.not_eof() {
        state.add_ins()?;
    }
    Ok(state.bytecode)
}
