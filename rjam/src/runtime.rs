pub use crate::bytecode::{Bytecode, ins};
pub use crate::value::{Char, Value, FromValue};

pub struct Runtime {
    stack: Vec<Value>,
}

impl Runtime {
    fn push(&mut self, v: impl Into<Value>) {
        self.stack.push(v.into());
    }

    pub fn new() -> Self {
        Runtime { stack: Vec::new() }
    }

    pub fn pop(&mut self) -> Value {
        self.stack.pop().unwrap()
    }

    pub fn run(&mut self, bc: &Bytecode) {
        for &b in &bc.bytes {
            match b {
                ins::PLUS => {
                    let a: Value = self.pop();
                    let b: Value = self.pop();
                    binary_match!((a, b) {
                        (a: i64, b: i64) => self.push(a + b),
                    });
                }
                ins::ONE => self.push(1),
                b => panic!("invalid byte: 0x{:x}", b),
            }
        }
    }

    pub fn print_stack(&self) {
        let mut s = String::new();
        for (i, v) in self.stack.iter().enumerate() {
            v.repr(&mut s);
            print!("{}{}", if i == 0 { "" } else { " " }, s);
            s.clear();
        }
        println!();
    }
}
