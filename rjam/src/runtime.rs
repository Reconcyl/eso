pub use crate::bytecode::{Bytecode, Opcode};
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
        let mut const_idx = 0;
        for &b in &bc.bytes {
            use Opcode::*;
            let op = match Opcode::from_byte(b) {
                Some(op) => op,
                None => unreachable!("0x{:02x} is not a valid opcode", b),
            };
            match op {
                Lit => {
                    self.push(bc.consts[const_idx].clone());
                    const_idx += 1;
                }
                One => self.push(1),

                Not => {
                    let a = self.pop();
                    self.push(!a.truthiness().unwrap() as i64)
                }
                LowerA => {
                    let a = self.pop();
                    self.push(vec![a]);
                }

                Plus => {
                    let b = self.pop();
                    let a = self.pop();
                    binary_match!((a, b) {
                        (a: Char, b: Char) => // char char +
                            self.push(vec![a.into(), b.into()]),
                        [a: Char, b: ScalarToInt] => // char num +, num char +
                            self.push(Char(a.0.wrapping_add(b.0 as u32))),
                        (a: i64, b: i64) => // int int +
                            self.push(a + b),
                        [a: f64, b: NumToReal] => // int num +, num int +
                            self.push(a + b.0),
                        (a: Vec<Value>, b: Vec<Value>) => // arr arr +
                            {
                                let mut a = a;
                                let mut b = b;
                                a.append(&mut b);
                                self.push(a);
                            },
                        (a: Value, b: Vec<Value>) => // any arr +
                            {
                                let mut b = b;
                                b.insert(0, a);
                                self.push(b);
                            },
                        (a: Vec<Value>, b: Value) => // arr any +
                            {
                                let mut a = a;
                                a.push(b);
                                self.push(a);
                            },
                    });
                }
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
