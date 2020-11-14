pub use crate::bytecode::{Bytecode, ins};
pub use crate::value::{Char, Value, FromValue, ScalarToInt, NumToReal};

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
                        (a: Char, b: Char) =>
                            self.push(vec![a.into(), b.into()]),
                        [a: Char, b: ScalarToInt] =>
                            self.push(Char(a.0.wrapping_add(b.0 as u32))),
                        (a: i64, b: i64) => self.push(a + b),
                        [a: f64, b: NumToReal] => self.push(a + b.0),
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
