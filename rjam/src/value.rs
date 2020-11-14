use std::fmt::Write as _;
use std::rc::Rc;

use crate::bytecode::Bytecode;

/// A (possibly invalid) Unicode code point. We use this instead of
/// `char` so that character arithmetic never results in an error.
#[derive(Clone, Copy)]
pub struct Char(pub u32);

impl Char {
    pub fn to_char(self) -> char {
        std::char::from_u32(self.0)
            .unwrap_or(std::char::REPLACEMENT_CHARACTER)
    }
}

pub enum Value {
    Char(Char),
    Int(i64), // TODO: use bigint
    Real(f64),
    Array(Vec<Value>), // TODO: use im::Vector
    Block(Rc<Bytecode>),
}

impl Value {
    fn is_char(&self) -> bool {
        match *self {
            Value::Char(_) => true,
            _ => false,
        }
    }

    pub fn is_string(&self) -> bool {
        match *self {
            Value::Array(ref vs) => vs.iter().all(Value::is_char),
            _ => false,
        }
    }

    pub fn repr(&self, s: &mut String) {
        match self {
            Value::Char(c) => { s.push('\''); s.push(c.to_char()) }
            Value::Int(i) => { write!(s, "{}", i).unwrap() }
            Value::Real(f) => { write!(s, "{:.}", f).unwrap() }
            Value::Array(a) => {
                s.push('[');
                for (i, v) in a.iter().enumerate() {
                    if i != 0 {
                        s.push(' ');
                    }
                    v.repr(s);
                }
                s.push(']');
            }
            Value::Block(_) => s.push_str("{...}"), // TODO visualize this better
        }
    }
}

impl Into<Value> for Char {
    fn into(self) -> Value {
        Value::Char(self)
    }
}

impl Into<Value> for i64 {
    fn into(self) -> Value {
        Value::Int(self)
    }
}

pub trait FromValue: Sized {
    fn matches(value: &Value) -> bool;
    fn from_value(value: Value) -> Option<Self>;
}

impl FromValue for Char {
    fn matches(v: &Value) -> bool {
        matches!(v, Value::Char(_))
    }

    fn from_value(v: Value) -> Option<Self> {
        match v {
            Value::Char(c) => Some(c),
            _ => None,
        }
    }
}

impl FromValue for i64 {
    fn matches(v: &Value) -> bool {
        matches!(v, Value::Int(_))
    }

    fn from_value(v: Value) -> Option<Self> {
        match v {
            Value::Int(i) => Some(i),
            _ => None,
        }
    }
}

impl FromValue for Value {
    fn matches(_: &Value) -> bool {
        true
    }

    fn from_value(v: Value) -> Option<Self> {
        Some(v)
    }
}

/// Attempt to downcast a pair of values to the provided types.
pub fn try_downcast_2<T: FromValue, U: FromValue>(a: Value, b: Value)
    -> Option<(T, U)>
{
    if T::matches(&a) && U::matches(&b) {
        T::from_value(a).zip(U::from_value(b))
    } else {
        None
    }
}

macro_rules! binary_match {
    // base case: no cases remaining
    (($scrut_a:expr, $scrut_b:expr) {}) => {{}};

    // base case: wildcard
    (($scrut_a:expr, $scrut_b:expr) {
        _ => $e:expr
    }) => { $e };

    // recursive case: match a particular pair of types
    (($scrut_a:expr, $scrut_b:expr) {
        ($a:ident : $T:ty, $b:ident : $U:ty) => $e:expr,
        $($rest:tt)*
    }) => {{
        // to avoid re-evaluation
        let scrut_a = $scrut_a;
        let scrut_b = $scrut_b;
        // do the types match?
        match crate::value::try_downcast_2::<$T, $U>(scrut_a, scrut_b) {
            Some(($a, $b)) => $e,
            None => binary_match!((scrut_a, scrut_b) { $($rest)* })
        }
    }};

    // recursive case: match a particular pair of types in any order
    (($scrut_a:expr, $scrut_b:expr) {
        [$a:ident : $T:ty, $b:ident : $U:ty] => $e:expr,
        $($rest:tt)*
    }) => {
        binary_match!(($scrut_a, $scrut_b) {
            ($a: $T, $b: $U) => $e,
            ($b: $U, $a: $T) => $e,
            $($rest)*
        })
    }
}
