use std::cmp::Ordering;
use std::fmt::Write as _;
use std::rc::Rc;

use crate::bytecode::Bytecode;
use crate::utils::f64_total_cmp;

/// A (possibly invalid) Unicode code point. We use this instead of
/// `char` so that character arithmetic never results in an error.
#[derive(Clone, Copy)]
pub struct Char(pub u32);

impl Char {
    pub fn to_std_char(self) -> char {
        std::char::from_u32(self.0)
            .unwrap_or(std::char::REPLACEMENT_CHARACTER)
    }
}

pub type Block = Rc<Bytecode>;
pub type Array = im::vector::Vector<Value>;

#[derive(Clone)]
pub enum Value {
    Char(Char),
    Int(i64), // TODO: use bigint
    Real(f64),
    Array(Array),
    Block(Block),
}

impl Value {
    /// A very strict version of equality used by the `#` operator.
    pub fn find_eq(&self, other: &Value) -> bool {
        match (self, other) {
            (Self::Char(a), Self::Char(b)) => a.0 == b.0,
            (Self::Int(a), Self::Int(b)) => a == b,
            (Self::Real(a), Self::Real(b)) => a == b,
            (Self::Array(a), Self::Array(b)) =>
                a.len() == b.len()
                && a.iter().zip(b).all(|(ai, bi)| ai.find_eq(bi)),
            (Self::Block(a), Self::Block(b)) => Rc::ptr_eq(a, b),
            _ => false,
        }
    }

    pub fn truthiness(&self) -> Option<bool> {
        match *self {
            Value::Char(c) => Some(c.0 > 0),
            Value::Int(i) => Some(i != 0),
            Value::Real(x) => Some(x != 0.0),
            Value::Array(ref a) => Some(!a.is_empty()),
            Value::Block(_) => None,
        }
    }

    fn is_char(&self) -> bool {
        match *self {
            Value::Char(_) => true,
            _ => false,
        }
    }

    pub fn repr(&self, s: &mut String) {
        match self {
            Value::Char(c) => { s.push('\''); s.push(c.to_std_char()) }
            Value::Int(i) => { write!(s, "{}", i).unwrap() }
            Value::Real(f) => { write!(s, "{:.}", f).unwrap() }
            Value::Array(a) => {
                // is this array a string?
                if a.iter().all(Value::is_char) {
                    s.push('"');
                    for c in a {
                        if let Value::Char(c) = c {
                            let c = c.to_std_char();
                            // TODO: CJam doesn't put these where they
                            // aren't necessary, so we shouldn't either
                            if c == '\\' || c == '"' {
                                s.push('\\');
                            }
                            s.push(c);
                        } else {
                            unreachable!()
                        }
                    }
                    s.push('"');
                } else {
                    s.push('[');
                    for (i, v) in a.iter().enumerate() {
                        if i != 0 {
                            s.push(' ');
                        }
                        v.repr(s);
                    }
                    s.push(']');
                }
            }
            Value::Block(_) => s.push_str("{...}"), // TODO visualize this better
        }
    }
}

/// The ordering used for sorting and comparison.
impl Ord for Value {
    fn cmp(&self, other: &Value) -> Ordering {
        use Value::*;
        match (self, other) {
            // scalar types are comparable with each other

            (&Char(a), &Char(b)) => a.0.cmp(&b.0),
            (&Char(a), &Int(b)) => (a.0 as i64).cmp(&b),
            (&Char(a), &Real(b)) => f64_total_cmp(a.0 as f64, b),

            (&Int(a), &Char(b)) => a.cmp(&(b.0 as i64)),
            (&Int(a), &Int(b)) => a.cmp(&b),
            (&Int(a), &Real(b)) => f64_total_cmp(a as f64, b),

            (&Real(a), &Char(b)) => f64_total_cmp(a, b.0 as f64),
            (&Real(a), &Int(b)) => f64_total_cmp(a, b as f64),
            (&Real(a), &Real(b)) => f64_total_cmp(a, b),

            // blocks are greater than all other types but equal to each other

            (&Block(_), &Block(_)) => Ordering::Equal,
            (&Block(_), _) => Ordering::Greater,
            (_, &Block(_)) => Ordering::Less,

            // arrays are greater than scalars and comparable
            // with each other lexicographically

            (&Array(ref a), &Array(ref b)) => a.cmp(b),
            (&Array(_), _) => Ordering::Greater,
            (_, &Array(_)) => Ordering::Less,
        }
    }
}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Value) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Eq for Value {}

impl PartialEq for Value {
    fn eq(&self, other: &Value) -> bool {
        self.cmp(other) == Ordering::Equal
    }
}

impl Into<Value> for char {
    fn into(self) -> Value {
        Value::Char(Char(self as u32))
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

impl Into<Value> for f64 {
    fn into(self) -> Value {
        Value::Real(self)
    }
}

impl Into<Value> for Array {
    fn into(self) -> Value {
        Value::Array(self)
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

impl FromValue for f64 {
    fn matches(v: &Value) -> bool {
        matches!(v, Value::Real(_))
    }

    fn from_value(v: Value) -> Option<Self> {
        match v {
            Value::Real(x) => Some(x),
            _ => None,
        }
    }
}

impl FromValue for Array {
    fn matches(v: &Value) -> bool {
        matches!(v, Value::Array(_))
    }

    fn from_value(v: Value) -> Option<Self> {
        match v {
            Value::Array(a) => Some(a),
            _ => None,
        }
    }
}

impl FromValue for Block {
    fn matches(v: &Value) -> bool {
        matches!(v, Value::Block(_))
    }

    fn from_value(v: Value) -> Option<Self> {
        match v {
            Value::Block(b) => Some(b),
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

/// Matches any scalar (char/numeric) type.
pub struct Scalar(pub Value);

impl FromValue for Scalar {
    fn matches(v: &Value) -> bool {
        matches!(v, Value::Char(_) | Value::Int(_) | Value::Real(_))
    }

    fn from_value(v: Value) -> Option<Self> {
        Some(v).filter(Self::matches).map(Self)
    }
}

/// Matches any scalar type and converts it to an integer.
pub struct ScalarToInt(pub i64);

impl FromValue for ScalarToInt {
    fn matches(v: &Value) -> bool {
        matches!(v, Value::Char(_) | Value::Int(_) | Value::Real(_))
    }

    fn from_value(v: Value) -> Option<Self> {
        match v {
            Value::Char(c) => Some(Self(c.0 as i64)),
            Value::Int(i) => Some(Self(i)),
            Value::Real(x) => Some(Self(x as i64)),
            _ => None,
        }
    }
}

/// Matches any numeric type and converts it to a float.
pub struct NumToReal(pub f64);

impl FromValue for NumToReal {
    fn matches(v: &Value) -> bool {
        matches!(v, Value::Int(_) | Value::Real(_))
    }

    fn from_value(v: Value) -> Option<Self> {
        match v {
            Value::Int(i) => Some(Self(i as f64)),
            Value::Real(x) => Some(Self(x)),
            _ => None,
        }
    }
}

/// Attempt to downcast a pair of values to the provided types.
pub fn try_downcast_2<T: FromValue, U: FromValue>(a: Value, b: Value)
    -> Result<(T, U), (Value, Value)>
{
    if T::matches(&a) && U::matches(&b) {
        T::from_value(a).zip(U::from_value(b))
            .ok_or_else(|| panic!("invalid `matches()` method"))
    } else {
        Err((a, b))
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
        // do the types match?
        match crate::value::try_downcast_2::<$T, $U>($scrut_a, $scrut_b) {
            Ok(($a, $b)) => $e,
            Err((_a, _b)) => binary_match!((_a, _b) { $($rest)* })
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
