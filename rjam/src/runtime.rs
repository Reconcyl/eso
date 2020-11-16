use std::convert::TryFrom;
use std::fmt;

use crate::bytecode::{Bytecode, Opcode};
use crate::value::{Char, Array, Block, Value, FromValue, Scalar, ScalarToInt, NumToReal};
use crate::utils::{get_wrapping, try_position};

pub enum Error {
    BadOpcode(u8),
    PopEmpty,
    PickEmpty,
    NoBlockTruthiness,
    ModByZero,
    Type {
        ex: &'static str,
        got: &'static str,
        op: &'static str,
    },
    NotHandled1 {
        got: &'static str,
        op: &'static str,
    },
    NotHandled2 {
        got1: &'static str,
        got2: &'static str,
        op: &'static str,
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::BadOpcode(b) =>
                write!(f, "encountered invalid opcode `0x{:02x}` (this is a bug)", b),
            Self::PopEmpty =>
                write!(f, "attempted to pop from empty stack"),
            Self::PickEmpty =>
                write!(f, "attempted to pick from empty stack"),
            Self::NoBlockTruthiness =>
                write!(f, "attempted to cast block to bool"),
            Self::ModByZero =>
                write!(f, "% by zero"),
            Self::Type { ex, got, op } =>
                write!(f, "`{}` expected {}, got {}", op, ex, got),
            Self::NotHandled1 { got, op } =>
                write!(f, "`{} {}` not handled", got, op),
            Self::NotHandled2 { got1, got2, op } =>
                write!(f, "`{} {} {}` not handled", got1, got2, op),
        }
    }
}

pub struct Runtime {
    stack: Vec<Value>,
}

impl Runtime {
    fn push(&mut self, v: impl Into<Value>) {
        self.stack.push(v.into());
    }

    fn pop(&mut self) -> Result<Value, Error> {
        self.stack.pop().ok_or(Error::PopEmpty)
    }

    fn pop_typed<T: FromValue>(&mut self, op: &'static str) -> Result<T, Error> {
        let val = self.pop()?;
        let got = val.type_name();
        T::from_value(val).ok_or_else(
            || Error::Type { ex: T::description(), got, op })
    }

    fn copy_elem(&mut self, i: i64) -> Result<(), Error> {
        if let Some(e) = get_wrapping(&self.stack, i) {
            let val = e.clone();
            self.push(val);
            Ok(())
        } else {
            Err(Error::PickEmpty)
        }
    }

    pub fn new() -> Self {
        Runtime { stack: Vec::new() }
    }

    pub fn run(&mut self, bc: &Bytecode) -> Result<(), Error> {
        let mut consts = bc.consts.iter();
        for &b in &bc.bytes {
            match Opcode::from_byte(b) {
                Some(op) => self.run_opcode(op, &mut consts)?,
                None => return Err(Error::BadOpcode(b)),
            };
        }
        Ok(())
    }

    fn run_opcode<'a>(
        &mut self,
        op: Opcode,
        consts: &mut impl Iterator<Item=&'a Value>,
    ) -> Result<(), Error> {
        use Opcode::*;
        match op {
            Lit => {
                self.push(consts.next().unwrap().clone());
            }

            One => self.push(1),

            Excl => {
                let a = self.pop()?;
                self.push(!a.truthiness().unwrap() as i64);
            }

            Dollar => {
                match self.pop()? {
                    Value::Int(i) => self.copy_elem(!i)?,
                    Value::Real(x) => self.copy_elem(!(x as i64))?,
                    Value::Array(mut a) => {
                        a.sort();
                        self.push(a);
                    }
                    Value::Block(b) => {
                        // this algorithm is mostly borrowed from
                        // `<&mut [T]>::sort_by_cached_key()`
                        let mut a = self.pop_typed::<Array>("$")?;
                        // evaluate the block for each element in the array
                        let mut indices = a.iter().enumerate()
                            .map(|(i, e)| {
                                self.push(e.clone());
                                self.run(&b)?; // TODO attach context information
                                Ok((self.pop()?, i))
                            })
                            .collect::<Result<Vec<_>, _>>()?;
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
                    v => return Err(Error::NotHandled1 {
                        op: "$",
                        got: v.type_name(),
                    }),
                }
            }

            Percent => {
                let b = self.pop()?;
                let a = self.pop()?;
                binary_match!((a, b) {
                    (a: i64, b: i64) => // int int %
                        if b == 0 {
                            return Err(Error::ModByZero);
                        } else {
                            self.push(a % b);
                        },
                    (a: NumToReal, b: NumToReal) => // int num %
                        self.push(a.0 % b.0),
                })
            }

            LowerA => {
                let a = self.pop()?;
                self.push(im::vector![a]);
            }

            Hash => {
                let b = self.pop()?;
                let a = self.pop()?;
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
                        match try_position(
                            a.into_iter(),
                            |elem| {
                                self.push(elem);
                                self.run(&b)?; // TODO attach context information
                                self.pop()?.truthiness()
                                    .ok_or(Error::NoBlockTruthiness)
                            })?
                        {
                            Some(i) => self.push(i as i64),
                            None => self.push(-1),
                        },
                });
            }

            Plus => {
                let b = self.pop()?;
                let a = self.pop()?;
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
                    (a: Value, b: Value) => // error
                        return Err(Error::NotHandled2 {
                            got1: a.type_name(),
                            got2: b.type_name(),
                            op: "+",
                        }),
                });
            }
        }
        Ok(())
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
