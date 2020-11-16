use crate::bytecode::{Bytecode, Opcode};
use crate::value::{Array, Value};

/// An error encountered while parsing.
#[derive(Debug)]
pub enum Error {
    Unexpected {
        ex: Expect,
        got: Outcome,
    }
}

/// A description of a particular thing the parser could be expecting.
#[derive(Debug, Clone, Copy)]
pub enum Expect {
    Instr,
    StrLiteral,
    CharLiteral,
}

/// A description of a particular malformed construct that the parser
/// could encounter.
#[derive(Debug, Clone, Copy)]
pub enum Outcome {
    Byte(u8),
    InvalidUtf8([u8; 4]),
    Eof,
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
    fn next_byte(&mut self) -> Option<u8> {
        let res = self.code.get(self.pos).copied();
        if res.is_some() {
            self.pos += 1;
        }
        res
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
                    got: Outcome::InvalidUtf8(buf),
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
        match self.next_byte().unwrap() {
            b'\t' | b'\n' | b'\r' | b' ' => {}
            b'!' => self.add_opcode(Excl),
            b'"' => {
                let val = self.next_str()?;
                self.add_lit(val);
            }
            b'#' => self.add_opcode(Hash),
            b'$' => self.add_opcode(Dollar),
            b'\'' => {
                let c = self.next_char(Expect::CharLiteral)?;
                self.add_lit(c.into());
            }
            b'+' => self.add_opcode(Plus),
            b'1' => self.add_opcode(One),
            b'a' => self.add_opcode(LowerA),
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
        bytecode: Bytecode {
            bytes: Vec::new(),
            consts: Vec::new(),
        }
    };
    while state.not_eof() {
        state.add_ins()?;
    }
    Ok(state.bytecode)
}
