use crate::bytecode::{Bytecode, ins};

pub type Error = ();

/// Parse the given program into bytecode form.
pub fn parse(code: &[u8]) -> Result<Bytecode, Error> {
    let mut bytes = Vec::new();
    for byte in code {
        match byte {
            b'h' => bytes.push(ins::HELLO_WORLD),
            _ => return Err(()),
        }
    }
    Ok(Bytecode { bytes })
}
