pub use crate::bytecode::{Bytecode, ins};
pub use crate::value::{Value, FromValue};

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

    pub fn pop<T: for<'a> FromValue<'a>>(&mut self) -> T {
        T::from_value(&self.stack.pop().unwrap()).unwrap()
    }

    pub fn run(&mut self, bc: &Bytecode) {
        for &b in &bc.bytes {
            match b {
                ins::PLUS => {
                    let a: i64 = self.pop();
                    let b: i64 = self.pop();
                    self.push(a + b);
                }
                ins::ONE => self.push(1),
                b => panic!("invalid byte: 0x{:x}", b),
            }
        }
    }

    pub fn print_stack(&self) {
        for (i, n) in self.stack.iter().enumerate() {
            print!("{}{:?}", if i == 0 { "" } else { " " }, n);
        }
        println!();
    }
}
