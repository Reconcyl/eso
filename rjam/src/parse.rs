pub type Bytecode = Vec<u8>;
pub type Error = ();

/// Parse the given program into bytecode form.
pub fn parse(code: &[u8]) -> Result<Bytecode, Error> {
    let mut bc = Bytecode::new();
    for byte in code {
        match byte {
            b'h' => bc.push(0x00),
            _ => return Err(()),
        }
    }
    Ok(bc)
}
