pub struct Bytecode {
    pub bytes: Vec<u8>,
}

pub mod ins {
    pub const NOT:     u8 = 0x00;
    pub const PLUS:    u8 = 0x01;
    pub const ONE:     u8 = 0x02;
    pub const LOWER_A: u8 = 0x03;
}
