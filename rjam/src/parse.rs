use crate::bytecode::{Bytecode, ins};

pub type Error = ();

/// Parse the given program into bytecode form.
pub fn parse(code: &[u8]) -> Result<Bytecode, Error> {
    let mut bytes = Vec::new();
    for byte in code {
        match byte {
            b'\t' | b'\n' | b'\r' | b' ' => {}
            b'!' => bytes.push(ins::NOT),
            b'+' => bytes.push(ins::PLUS),
            b'1' => bytes.push(ins::ONE),
            b'a' => bytes.push(ins::LOWER_A),
            _ => return Err(()),
        }
    }
    Ok(Bytecode { bytes })
}
