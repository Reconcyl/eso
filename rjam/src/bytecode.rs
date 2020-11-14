pub struct Bytecode {
    pub bytes: Vec<u8>,
}

pub mod ins {
    pub const PLUS: u8 = 0x00;
    pub const ONE: u8 = 0x01;
}
