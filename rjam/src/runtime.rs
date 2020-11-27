use num_bigint::Sign;
use num_traits::{Zero as _, ToPrimitive as _};

use std::collections::HashSet;
use std::fmt;
use std::rc::Rc;

use crate::bytecode::{Bytecode, Opcode};
use crate::value::{
    Char, Int, Array, Block, Value, Hashable, FromValue,
    Scalar, NumToReal, NumToInt, IntegralToChar,
};
use crate::utils::{
    reverse_vector, get_wrapping, try_position, try_retain,
    bigint_to_u32_wrapping, split_iter_one, split_iter_many
};

pub enum Error {
    BadOpcode(u8),

    PeekEmpty,
    PickEmpty,
    PopEmpty,

    ExponentTooLarge(Int),
    IntOfInf,
    ModByZero,
    MulBadArrayLength,
    NoBlockTruthiness,
    PopEmptyArray,
    RangeNegative,
    RangeTooLarge,
    ReduceEmptyArray,

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
            Self::PeekEmpty =>
                write!(f, "attempted to peek from empty stack"),
            Self::PickEmpty =>
                write!(f, "attempted to pick from empty stack"),
            Self::PopEmpty =>
                write!(f, "attempted to pop from empty stack"),
            Self::ExponentTooLarge(e) =>
                write!(f, "exponent is too large: {}", e),
            Self::IntOfInf =>
                write!(f, "non-finite floats cannot be coerced to int"),
            Self::ModByZero =>
                write!(f, "% by zero"),
            Self::MulBadArrayLength =>
                write!(f, "cannot construct array of the specified length"),
            Self::NoBlockTruthiness =>
                write!(f, "attempted to cast block to bool"),
            Self::PopEmptyArray =>
                write!(f, "attempted to pop from empty array"),
            Self::RangeNegative =>
                write!(f, "cannot construct negative range"),
            Self::RangeTooLarge =>
                write!(f, "number is too large for range"),
            Self::ReduceEmptyArray =>
                write!(f, "attempted to reduce over empty array"),
            Self::Type { ex, got, op } =>
                write!(f, "{} expected {}, got {}", op, ex, got),
            Self::NotHandled1 { got, op } =>
                write!(f, "{} {} not handled", got, op),
            Self::NotHandled2 { got1, got2, op } =>
                write!(f, "{} {} {} not handled", got1, got2, op),
        }
    }
}

pub struct Runtime {
    stack: Vec<Value>,
    /// The positions of flags placed on the
    /// stack to mark the start of arrays.
    flags: Vec<usize>,
}

impl Runtime {
    fn push(&mut self, v: impl Into<Value>) {
        self.stack.push(v.into());
    }

    /// Remove the top value from the stack.
    fn pop(&mut self) -> Result<Value, Error> {
        if let Some(val) = self.stack.pop() {
            let height = self.stack.len();
            for flag_pos in self.flags.iter_mut().rev() {
                // positions are stored in sorted order, so this
                // search will always find all such elements
                if *flag_pos > height {
                    *flag_pos = height;
                } else {
                    break
                }
            }
            Ok(val)
        } else {
            Err(Error::PopEmpty)
        }
    }

    /// Return the top value on the stack.
    fn peek(&self) -> Result<&Value, Error> {
        self.stack.last().ok_or(Error::PeekEmpty)
    }

    fn pop_typed<T: FromValue>(&mut self, op: &'static str) -> Result<T, Error> {
        let val = self.pop()?;
        let got = val.type_name();
        T::from_value(val).ok_or_else(
            || Error::Type { ex: T::description(), got, op })
    }

    fn copy_elem(&mut self, i: Int) -> Result<(), Error> {
        if let Some(e) = get_wrapping(&self.stack, -(i + 1u8)) {
            let val = e.clone();
            self.push(val);
            Ok(())
        } else {
            Err(Error::PickEmpty)
        }
    }

    /// Add a flag to mark the start of an array literal.
    fn begin_array(&mut self) {
        self.flags.push(self.stack.len());
    }

    /// Remove all elements pushed to the stack since the most recent flag
    /// and collect them into an array.
    fn end_array(&mut self) {
        let flag = self.flags.pop().unwrap_or(0);
        let elems: Array = self.stack.drain(flag..).collect();
        self.push(elems);
    }

    pub fn new() -> Self {
        Runtime {
            stack: Vec::new(),
            flags: Vec::new(),
        }
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
            Lit => { self.push(consts.next().unwrap().clone()); Ok(()) }
            LeftBracket => { self.begin_array(); Ok(()) }
            RightBracket => { self.end_array(); Ok(()) }

            Excl => self.op_excl(),
            Dollar => self.op_dollar(),
            LeftParen => self.op_left_paren(),
            RightParen => self.op_right_paren(),
            Comma => self.op_comma(),
            Underscore => self.op_underscore(),
            LowerA => self.op_lower_a(),

            Percent => self.op_percent(),
            Hash => self.op_hash(),
            And => self.op_and(),
            Star => self.op_star(),
            Plus => self.op_plus(),
            Minus => self.op_minus(),
        }
    }

    fn op_excl(&mut self) -> Result<(), Error> {
        let a = self.pop()?;
        let bool_val = !a.truthiness().ok_or(Error::NoBlockTruthiness)?;
        self.push(Int::from(bool_val as u8));
        Ok(())
    }

    fn op_dollar(&mut self) -> Result<(), Error> {
        match self.pop()? {
            Value::Int(i) => self.copy_elem(i)?,
            Value::Real(x) =>
                if x.is_finite() {
                    self.copy_elem(Int::from(x as i64))?;
                } else {
                    return Err(Error::IntOfInf);
                }
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
                got: v.type_name(),
                op: "$",
            }),
        }
        Ok(())
    }

    fn op_left_paren(&mut self) -> Result<(), Error> {
        match self.pop()? {
            Value::Char(c) => self.push(Char(c.0.wrapping_sub(1))),
            Value::Int(i) => self.push(i - 1),
            Value::Real(x) => self.push(x - 1.),
            Value::Array(mut a) => {
                let first = a.pop_front().ok_or(Error::PopEmptyArray)?;
                self.push(a);
                self.push(first);
            }
            v => return Err(Error::NotHandled1 {
                got: v.type_name(),
                op: "(",
            })
        }
        Ok(())
    }

    fn op_right_paren(&mut self) -> Result<(), Error> {
        match self.pop()? {
            Value::Char(c) => self.push(Char(c.0.wrapping_add(1))),
            Value::Int(i) => self.push(i + 1),
            Value::Real(x) => self.push(x + 1.),
            Value::Array(mut a) => {
                let last = a.pop_back().ok_or(Error::PopEmptyArray)?;
                self.push(a);
                self.push(last);
            }
            v => return Err(Error::NotHandled1 {
                got: v.type_name(),
                op: ")",
            })
        }
        Ok(())
    }

    fn op_comma(&mut self) -> Result<(), Error> {
        match self.pop()? {
            Value::Char(c) =>
                self.push((0..c.0)
                    .map(|c| Value::Char(Char(c)))
                    .collect::<Array>()),
            Value::Int(i) =>
                if let Some(i) = i.to_u64() {
                    self.push((0..i)
                        .map(|j| Int::from(j).into())
                        .collect::<Array>())
                } else if i.sign() == Sign::Minus {
                    return Err(Error::RangeNegative);
                } else {
                    return Err(Error::RangeTooLarge);
                }
            Value::Real(x) =>
                if x.is_finite() {
                    self.push((0..x as u64)
                        .map(|j| Int::from(j).into())
                        .collect::<Array>())
                } else {
                    return Err(Error::IntOfInf);
                }
            Value::Array(a) =>
                self.push(Int::from(a.len())),
            Value::Block(b) => {
                let elems: Array = self.pop_typed(",")?;
                let filtered = try_retain(elems, |elem| {
                    self.push(elem.clone());
                    self.run(&b)?; // TODO attach context information
                    self.pop()?.truthiness()
                        .ok_or(Error::NoBlockTruthiness)
                })?;
                self.push(filtered);
            }
        }
        Ok(())
    }

    fn op_underscore(&mut self) -> Result<(), Error> {
        let a = self.peek()?.clone();
        self.push(a);
        Ok(())
    }

    fn op_lower_a(&mut self) -> Result<(), Error> {
        let a = self.pop()?;
        self.push(im::vector![a]);
        Ok(())
    }

    fn op_percent(&mut self) -> Result<(), Error> {
        let b = self.pop()?;
        let a = self.pop()?;
        binary_match!((a, b) {
            (a: Int, b: Int) => // int int %
                if b.is_zero() {
                    return Err(Error::ModByZero);
                } else {
                    self.push(a % b);
                },
            (a: NumToReal, b: NumToReal) => // real num %, num real %
                self.push(a.0 % b.0),
            (a: Array, b: Array) => // array array %
                self.push(
                    split_iter_many(&mut a.into_iter(), &b, b.len())
                        .filter(|part: &Array| !part.is_empty())
                        .map(Array::into)
                        .collect::<Array>()),
            [a: Array, b: Char] => // array char %, char array %
                self.push(
                    split_iter_one(&mut a.into_iter(), &b.into())
                        .filter(|part: &Array| !part.is_empty())
                        .map(Array::into)
                        .collect::<Array>()),
            [a: Array, b: NumToInt] => // array num %, num array %
                {
                    let mut a = a;
                    let b = b.0.ok_or(Error::IntOfInf)?;
                    match b.sign() {
                        Sign::NoSign => return Err(Error::ModByZero),
                        Sign::Plus => match b.to_u64() {
                            Some(1) => {}
                            Some(b) => {
                                let mut i = 0;
                                a.retain(|_| {
                                    let keep = i % b == 0;
                                    i += 1;
                                    keep
                                });
                            }
                            // no array could possibly have a length greater
                            // than a positive integer that doesn't fit in an
                            // `i64`, so we can just take the first element
                            None => a.truncate(1),
                        }
                        Sign::Minus => match b.to_i64() {
                            Some(-1) => reverse_vector(&mut a),
                            Some(b) => {
                                reverse_vector(&mut a);
                                let mut i = 0;
                                a.retain(|_| {
                                    let keep = i % -b == 0;
                                    i += 1;
                                    keep
                                });
                            }
                            // no array could possibly have a length greater
                            // than the magnitude of an integer that doesn't
                            // fit in an `i64`, so we can just take the last
                            // element
                            None => a = a.pop_back().into_iter().collect(),
                        }
                    }
                    self.push(a);
                },
            (a: Value, b: Value) => // error
                return Err(Error::NotHandled2 {
                    got1: a.type_name(),
                    got2: b.type_name(),
                    op: "%",
                }),
        });
        Ok(())
    }

    fn op_hash(&mut self) -> Result<(), Error> {
        let b = self.pop()?;
        let a = self.pop()?;
        binary_match!((a, b) {
            (a: Int, b: Int) => // int int #
                if let Some(pow) = b.to_u32() {
                    self.push(a.pow(pow));
                } else if a.is_zero() {
                    self.push(a);
                } else if let Some(pow) = b.to_i32() {
                    self.push(a.to_f64().unwrap().powi(pow));
                } else if b.sign() == Sign::Minus {
                    self.push(Int::zero());
                } else {
                    return Err(Error::ExponentTooLarge(b));
                },
            (a: f64, b: Int) => // real int #
                if let Some(pow) = b.to_i32() {
                    self.push(a.powi(pow))
                } else {
                    self.push(a.powf(b.to_f64().unwrap()));
                },
            (a: NumToReal, b: f64) => // num real #
                self.push(a.0.powf(b)),
            [a: Scalar, b: Array] => // scalar array #, array scalar #
                match b.iter().position(|e| e.strict_eq(&a.0)) {
                    Some(i) => self.push(Int::from(i)),
                    None => self.push(Int::from(-1)),
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
                                .all(|(ai, bi)| ai.strict_eq(bi)))
                    };
                    match idx {
                        Some(i) => self.push(Int::from(i)),
                        None => self.push(Int::from(-1)),
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
                    Some(i) => self.push(Int::from(i)),
                    None => self.push(Int::from(-1)),
                },
            (a: Value, b: Value) => // error
                return Err(Error::NotHandled2 {
                    got1: a.type_name(),
                    got2: b.type_name(),
                    op: "#"
                }),
        });
        Ok(())
    }

    fn op_and(&mut self) -> Result<(), Error> {
        let b = self.pop()?;
        let a = self.pop()?;
        binary_match!((a, b) {
            (a: Int, b: Int) => // int int &
                self.push(a & b),
            [a: Char, b: IntegralToChar] => // int char &, char int &, char char &
                self.push(Char(a.0 & b.0.0)),
            (a: Array, b: Array) => // array array &
                {
                    let mut a = a;
                    // keep the first copy of every element
                    // in `a` as long as it appears in `b`
                    let mut items: HashSet<_> = b.into_iter()
                        .map(Hashable).collect();
                    a.retain(|elem| {
                        items.remove(Hashable::from_ref(elem))
                    });
                    self.push(a);
                },
            [a: Scalar, b: Array] => // scalar array &, array scalar &
                {
                    let a = Hashable(a.0);
                    let present = b.iter().any(|elem|
                        Hashable::from_ref(elem) == &a);
                    let mut b = b;
                    b.clear();
                    if present {
                        b.push_back(a.0);
                    }
                    self.push(b);
                },
            (a: Value, b: Block) => // any block & (error if 1st is block)
                match a.truthiness() {
                    Some(true) => self.run(&b)?, // TODO attach context information
                    Some(false) => {}
                    None => return Err(Error::NotHandled2 {
                        got1: Value::BLOCK_TYPE_NAME,
                        got2: Value::BLOCK_TYPE_NAME,
                        op: "&",
                    })
                },
            (a: Value, b: Value) => // error
                return Err(Error::NotHandled2 {
                    got1: a.type_name(),
                    got2: b.type_name(),
                    op: "&",
                }),
        });
        Ok(())
    }

    fn op_star(&mut self) -> Result<(), Error> {
        let b = self.pop()?;
        let a = self.pop()?;
        binary_match!((a, b) {
            (a: Int, b: Int) => // int int *
                self.push(a * b),
            (a: NumToReal, b: NumToReal) => // real num *, num real *
                self.push(a.0 * b.0),
            [a: Array, b: NumToInt] => // array num *, num array *
                match b.0.ok_or(Error::IntOfInf)?.to_usize() {
                    Some(b) if b.checked_mul(a.len()).is_some() => {
                        let mut res = Array::new();
                        for _ in 0..b {
                            res.extend(a.iter().cloned());
                        }
                        self.push(res);
                    }
                    _ => return Err(Error::MulBadArrayLength),
                },
            [a: Char, b: NumToInt] => // char num *, num char *
                match b.0.ok_or(Error::IntOfInf)?.to_usize() {
                    None => return Err(Error::MulBadArrayLength),
                    Some(len) => {
                        let mut res = Array::new();
                        for _ in 0..len {
                            res.push_back(a.into());
                        }
                        self.push(res);
                    }
                },
            (a: Array, b: Array) => // array array *
                {
                    let mut res = Array::new();
                    for (i, elem) in a.into_iter().enumerate() {
                        if i != 0 {
                            res.extend(b.iter().cloned());
                        }
                        if let Value::Array(elem) = elem {
                            res.append(elem);
                        } else {
                            res.push_back(elem);
                        }
                    }
                    self.push(res);
                },
            [a: Array, b: Char] => // array char *, char array *
                {
                    let mut res = Array::new();
                    for (i, elem) in a.into_iter().enumerate() {
                        if i != 0 {
                            res.push_back(b.into());
                        }
                        if let Value::Array(elem) = elem {
                            res.append(elem);
                        } else {
                            res.push_back(elem);
                        }
                    }
                    self.push(res);
                },
            [a: Block, b: NumToInt] => // block num *, num block *
                if let Some(b) = b.0.ok_or(Error::IntOfInf)?.to_isize() {
                    for _ in 0..b {
                        self.run(&a)?; // TODO attach context information
                    }
                } else {
                    loop {
                        self.run(&a)?; // TODO attach context information
                    }
                },
            [a: Array, b: Block] => // array block *, block array *
                {
                    let mut a = a;
                    let first = a.pop_front().ok_or(Error::ReduceEmptyArray)?;
                    self.push(first);
                    for val in a {
                        self.push(val);
                        self.run(&b)?; // TODO attach context information
                    }
                },
            (a: Value, b: Value) => // error
                return Err(Error::NotHandled2 {
                    got1: a.type_name(),
                    got2: b.type_name(),
                    op: "*",
                }),
        });
        Ok(())
    }

    fn op_plus(&mut self) -> Result<(), Error> {
        let b = self.pop()?;
        let a = self.pop()?;
        binary_match!((a, b) {
            (a: Char, b: Char) => // char char +
                self.push(im::vector![a.into(), b.into()]),
            [a: Char, b: NumToInt] => // char num +, num char +
                {
                    let b = bigint_to_u32_wrapping(&b.0.ok_or(Error::IntOfInf)?);
                    self.push(Char(a.0.wrapping_add(b)));
                },
            (a: Int, b: Int) => // int int +
                self.push(a + b),
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
            (a: Block, b: Block) => // block block +
                {
                    let mut a = a;
                    let a_ref = Rc::make_mut(&mut a);
                    match Rc::try_unwrap(b) {
                        Ok(mut b) => {
                            a_ref.bytes.append(&mut b.bytes);
                            a_ref.consts.append(&mut b.consts);
                        }
                        Err(b) => {
                            a_ref.bytes.extend(b.bytes.iter().cloned());
                            a_ref.consts.extend(b.consts.iter().cloned());
                        }
                    }
                    self.push(a);
                },
            (a: Block, b: Value) => // block any +
                {
                    let mut a = a;
                    let a_ref = Rc::make_mut(&mut a);
                    a_ref.bytes.push(Opcode::Lit as u8);
                    a_ref.consts.push(b);
                    self.push(a);
                },
            (a: Value, b: Block) => // any block +
                {
                    // TODO - maybe use a VecDeque or
                    // something so this isn't O(n)?
                    let mut b = b;
                    let b_ref = Rc::make_mut(&mut b);
                    b_ref.bytes.insert(0, Opcode::Lit as u8);
                    b_ref.consts.insert(0, a);
                    self.push(b);
                },
            (a: Value, b: Value) => // error
                return Err(Error::NotHandled2 {
                    got1: a.type_name(),
                    got2: b.type_name(),
                    op: "+",
                }),
        });
        Ok(())
    }

    fn op_minus(&mut self) -> Result<(), Error> {
        let b = self.pop()?;
        let a = self.pop()?;
        binary_match!((a, b) {
            (a: Int, b: Int) => // int int -
                self.push(a - b),
            (a: NumToReal, b: NumToReal) => // real int -, int real -, real real -
                self.push(a.0 - b.0),
            (a: Char, b: Char) => // char char -
                self.push(Int::from((a.0 as i32).wrapping_sub(b.0 as i32))),
            (a: Char, b: NumToInt) => // char num -
                {
                    let b = bigint_to_u32_wrapping(&b.0.ok_or(Error::IntOfInf)?);
                    self.push(Char(a.0.wrapping_sub(b)));
                },
            (a: Array, b: Scalar) => // arr scalar -
                {
                    let mut a = a;
                    a.retain(|e| !b.0.strict_eq(e));
                    self.push(a);
                },
            (a: Array, b: Array) => // arr arr -
                {
                    let mut a = a;
                    let items: HashSet<_> = b.into_iter()
                        .map(Hashable).collect();
                    a.retain(|e| !items.contains(Hashable::from_ref(e)));
                    self.push(a);
                },
            (a: Scalar, b: Array) => // scalar arr -
                if b.contains(&a.0) {
                    self.push(im::vector![]);
                } else {
                    self.push(im::vector![a.0]);
                },
        });
        Ok(())
    }

    pub fn print_stack(&self) {
        let mut s = String::new();
        for (i, v) in self.stack.iter().enumerate() {
            v.repr(&mut s);
            print!("{}{}", if i == 0 { "" } else { " " }, s);
            s.clear();
        }
        if !self.stack.is_empty() {
            println!();
        }
    }
}
