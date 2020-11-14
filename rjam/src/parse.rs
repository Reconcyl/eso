use crate::bytecode::{Bytecode, Opcode};

pub type Error = ();

/// Parse the given program into bytecode form.
pub fn parse(code: &[u8]) -> Result<Bytecode, Error> {
    let mut bytes = Vec::new();
    for byte in code {
        use Opcode::*;
        match byte {
            b'\t' | b'\n' | b'\r' | b' ' => {}
            b'!' => bytes.push(Not as u8),
            b'+' => bytes.push(Plus as u8),
            b'1' => bytes.push(One as u8),
            b'a' => bytes.push(LowerA as u8),
            _ => return Err(()),
        }
    }
    Ok(Bytecode { bytes })
}
