pub use crate::bytecode::{Bytecode, ins};

pub struct Runtime {
    stack: Vec<i64>,
}

impl Runtime {
    pub fn new() -> Self {
        Runtime { stack: Vec::new() }
    }
    pub fn run(&mut self, bc: &Bytecode) {
        for &b in &bc.bytes {
            match b {
                ins::PLUS => {
                    let a = self.stack.pop().unwrap();
                    let b = self.stack.pop().unwrap();
                    self.stack.push(a + b);
                }
                ins::ONE => self.stack.push(1),
                b => panic!("invalid byte: 0x{:x}", b),
            }
        }
    }
    pub fn print_stack(&self) {
        for (i, n) in self.stack.iter().enumerate() {
            print!("{}{}", if i == 0 { "" } else { " " }, n);
        }
        println!();
    }
}
