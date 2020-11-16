use std::convert::TryFrom;

use crate::bytecode::{Bytecode, Opcode};
use crate::value::{Char, Array, Block, Value, FromValue, Scalar, ScalarToInt, NumToReal};
use crate::utils::get_wrapping;

pub struct Runtime {
    stack: Vec<Value>,
}

impl Runtime {
    fn push(&mut self, v: impl Into<Value>) {
        self.stack.push(v.into());
    }

    fn pop(&mut self) -> Value {
        self.stack.pop().unwrap()
    }

    fn pop_typed<T: FromValue>(&mut self) -> T {
        T::from_value(self.pop()).expect("type error")
    }

    pub fn new() -> Self {
        Runtime { stack: Vec::new() }
    }

    fn copy_elem(&mut self, i: i64) {
        if let Some(e) = get_wrapping(&self.stack, i) {
            let val = e.clone();
            self.push(val);
        } else {
            panic!("cannot pick from empty stack")
        }
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

                Dollar => {
                    let a = self.pop();
                    match a {
                        Value::Int(i) => self.copy_elem(i),
                        Value::Real(x) => self.copy_elem(x as i64),
                        Value::Array(mut a) => {
                            a.sort();
                            self.push(a);
                        }
                        Value::Block(b) => {
                            // this algorithm is mostly borrowed from
                            // `<&mut [T]>::sort_by_cached_key()`
                            let mut a = self.pop_typed::<Array>();
                            // evaluate the block for each element in the array
                            let mut indices = a.iter().enumerate()
                                .map(|(i, e)| {
                                    self.push(e.clone());
                                    self.run(&b);
                                    (self.pop(), i)
                                })
                                .collect::<Vec<_>>();
                            // sort the indices to get the correct permutation
                            // (stability isn't relevant because the second
                            // elements of the tuple are always distinct)
                            indices.sort_unstable();
                            // move elements in `a` according to this permutation
                            for i in 0..a.len() {
                                let mut j = indices[i].1;
                                while j < i {
                                    j = indices[j].1;
                                }
                                indices[i].1 = j;
                                a.swap(i, j);
                            }
                            self.push(a);
                        }
                        _ => panic!("invalid type"),
                    }
                }

                LowerA => {
                    let a = self.pop();
                    self.push(im::vector![a]);
                }

                Hash => {
                    let b = self.pop();
                    let a = self.pop();
                    binary_match!((a, b) {
                        (a: i64, b: i64) => // int int #
                            if let Ok(pow) = u32::try_from(b) {
                                self.push(a.wrapping_pow(pow));
                            } else if let Ok(pow) = i32::try_from(b) {
                                self.push((a as f64).powi(pow));
                            } else {
                                self.push((a as f64).powf(b as f64));
                            },
                        (a: f64, b: i64) => // real int #
                            if let Ok(pow) = i32::try_from(b) {
                                self.push((a as f64).powi(pow))
                            } else {
                                self.push(a.powf(b as f64))
                            },
                        (a: NumToReal, b: f64) => // num real #
                            self.push(a.0.powf(b)),
                        [a: Scalar, b: Array] => // scalar array #, array scalar #
                            match b.iter().position(|e| e.find_eq(&a.0)) {
                                Some(i) => self.push(i as i64),
                                None => self.push(-1),
                            },
                        (a: Array, b: Array) => // array array #
                            {
                                let al = a.len();
                                let bl = b.len();
                                let idx = if bl > al {
                                    None
                                } else {
                                    (0 .. 1 + al - bl).find(|&i|
                                        a.iter()
                                            .zip(&b)
                                            .skip(i)
                                            .all(|(ai, bi)| ai.find_eq(bi)))
                                };
                                match idx {
                                    Some(i) => self.push(i as i64),
                                    None => self.push(-1),
                                }
                            },
                        [a: Array, b: Block] => // array block #, block array #
                            match a.into_iter()
                                .position(|elem| {
                                    self.push(elem);
                                    self.run(&b);
                                    self.pop().truthiness().unwrap()
                                })
                            {
                                Some(i) => self.push(i as i64),
                                None => self.push(-1),
                            },
                    });
                }

                Plus => {
                    let b = self.pop();
                    let a = self.pop();
                    binary_match!((a, b) {
                        (a: Char, b: Char) => // char char +
                            self.push(im::vector![a.into(), b.into()]),
                        [a: Char, b: ScalarToInt] => // char num +, num char +
                            self.push(Char(a.0.wrapping_add(b.0 as u32))),
                        (a: i64, b: i64) => // int int +
                            self.push(a.wrapping_add(b)),
                        [a: f64, b: NumToReal] => // int num +, num int +
                            self.push(a + b.0),
                        (a: Array, b: Array) => // arr arr +
                            {
                                let mut a = a;
                                a.append(b);
                                self.push(a);
                            },
                        (a: Value, b: Array) => // any arr +
                            {
                                let mut b = b;
                                b.push_front(a);
                                self.push(b);
                            },
                        (a: Array, b: Value) => // arr any +
                            {
                                let mut a = a;
                                a.push_back(b);
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
