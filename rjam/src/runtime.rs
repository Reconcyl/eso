pub use crate::bytecode::{Bytecode, ins};

pub struct Runtime;

impl Runtime {
    pub fn new() -> Self {
        Runtime
    }
    pub fn run(&mut self, bc: &Bytecode) {
        for &b in &bc.bytes {
            match b {
                ins::HELLO_WORLD => println!("Hello, World!"),
                b => panic!("invalid byte: 0x{:x}", b),
            }
        }
    }
}
