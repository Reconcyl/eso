pub use crate::parse::Bytecode;

pub struct Runtime;

impl Runtime {
    pub fn new() -> Self {
        Runtime
    }
    pub fn run(&mut self, bc: &Bytecode) {
        for &b in bc {
            match b {
                0x00 => println!("Hello, World!"),
                b => panic!("invalid byte: 0x{:x}", b),
            }
        }
    }
}
