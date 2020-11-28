pub use num_bigint::BigInt as Int;
use num_traits::{Zero as _, ToPrimitive as _};

use std::cmp::Ordering;
use std::fmt::Write as _;
use std::hash::{Hash, Hasher};
use std::rc::Rc;

use crate::bytecode::Bytecode;
use crate::utils::{
    f64_total_cmp, bigint_u32_cmp, bigint_f64_cmp,
    bigint_to_u32_wrapping,
};

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
pub type Array = im::Vector<Value>;

#[derive(Clone)]
pub enum Value {
    Char(Char),
    Int(Int),
    Real(f64),
    Array(Array),
    Block(Block),
}

impl Value {
    /// A very strict version of equality used by `Hashable` and the `#` operator.
    pub fn strict_eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Char(a), Self::Char(b)) => a.0 == b.0,
            (Self::Int(a), Self::Int(b)) => a == b,
            (Self::Real(a), Self::Real(b)) => a.to_bits() == b.to_bits(),
            (Self::Array(a), Self::Array(b)) =>
                a.len() == b.len()
                && a.iter().zip(b).all(|(ai, bi)| ai.strict_eq(bi)),
            (Self::Block(a), Self::Block(b)) => Rc::ptr_eq(a, b),
            _ => false,
        }
    }

    pub fn truthiness(&self) -> Option<bool> {
        match *self {
            Value::Char(c) => Some(c.0 > 0),
            Value::Int(ref i) => Some(!i.is_zero()),
            Value::Real(x) => Some(x != 0.0),
            Value::Array(ref a) => Some(!a.is_empty()),
            Value::Block(_) => None,
        }
    }

    pub fn repr(&self, s: &mut String) {
        match self {
            Value::Char(c) => { s.push('\''); s.push(c.to_std_char()) }
            Value::Int(i) => { write!(s, "{}", i).unwrap() }
            Value::Real(f) =>
                if f.is_nan() {
                    s.push_str("0d0/")
                } else if *f == std::f64::INFINITY {
                    s.push_str("1d0/")
                } else if *f == std::f64::NEG_INFINITY {
                    s.push_str("-1d0/")
                } else {
                    write!(s, "{:.}", f).unwrap()
                }
            Value::Array(a) => {
                // is this array a string?
                if a.iter().all(Char::matches) {
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

    pub const BLOCK_TYPE_NAME: &'static str = "Block";

    pub fn type_name(&self) -> &'static str {
        match self {
            Self::Char(_) => "Character",
            Self::Int(_) => "Integer",
            Self::Real(_) => "Float",
            Self::Array(_) => "Array",
            Self::Block(_) => Self::BLOCK_TYPE_NAME,
        }
    }
}

impl Default for Value {
    fn default() -> Self {
        Self::Int(Int::zero())
    }
}

/// The ordering used for sorting and comparison.
impl Ord for Value {
    fn cmp(&self, other: &Value) -> Ordering {
        use Value::*;
        match (self, other) {
            // scalar types are comparable with each other

            (&Char(a), &Char(b)) => a.0.cmp(&b.0),
            (&Char(a), &Int(ref b)) => bigint_u32_cmp(a.0, b),
            (&Char(a), &Real(b)) => f64_total_cmp(a.0 as f64, b),

            (&Int(ref a), &Char(b)) => bigint_u32_cmp(b.0, a).reverse(),
            (&Int(ref a), &Int(ref b)) => a.cmp(&b),
            (&Int(ref a), &Real(b)) => bigint_f64_cmp(b, &a).reverse(),

            (&Real(a), &Char(b)) => f64_total_cmp(a, b.0 as f64),
            (&Real(a), &Int(ref b)) => bigint_f64_cmp(a, &b),
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

/// A newtype wrapper around `Value` that provides yet
/// another interpretation of equality (this time used
/// to provide a reasonable `Hash` implementation)
#[repr(transparent)]
pub struct Hashable(pub Value);

/// Basic sanity checks to make sure `Hashable` has the
/// same size and alignment as `Value`.
mod hashable_static_assertions {
    use std::mem::{size_of, align_of};
    use super::{Hashable, Value};
    const _SIZE:  u8 = (size_of::<Hashable>()  == size_of::<Value>())  as u8 - 1;
    const _ALIGN: u8 = (align_of::<Hashable>() == align_of::<Value>()) as u8 - 1;
}

impl Hashable {
    pub fn from_ref(v: &Value) -> &Self {
        // I can't find any official documentation guaranteeing
        // that transmutes between references to values and their
        // `#[repr(transparent)]` wrappers is safe, but I hope it is...
        let ptr = v as *const Value as *const Self;
        unsafe { &*ptr }
    }
}

impl Ord for Hashable {
    fn cmp(&self, other: &Self) -> Ordering {
        fn hash<T: Hash>(val: &T) -> u64 {
            use std::collections::hash_map::DefaultHasher;
            let mut hasher = DefaultHasher::new();
            val.hash(&mut hasher);
            hasher.finish()
        }
        hash(self).cmp(&hash(other))
    }
}

impl PartialOrd for Hashable {
    fn partial_cmp(&self, other: &Hashable) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Eq for Hashable {}

impl PartialEq for Hashable {
    fn eq(&self, other: &Self) -> bool {
        self.0.strict_eq(&other.0)
    }
}

impl Hash for Hashable {
    fn hash<H: Hasher>(&self, state: &mut H) {
        fn inner<H: Hasher>(v: &Value, state: &mut H) {
            match v {
                Value::Char(c) => {
                    0u8.hash(state);
                    c.0.hash(state);
                }
                Value::Int(i) => {
                    1u8.hash(state);
                    i.hash(state);
                }
                Value::Real(x) => {
                    2u8.hash(state);
                    x.to_bits().hash(state);
                }
                Value::Array(a) => {
                    3u8.hash(state);
                    a.len().hash(state);
                    for elem in a {
                        inner(elem, state);
                    }
                }
                Value::Block(b) => {
                    4u8.hash(state);
                    Rc::as_ptr(b).hash(state);
                }
            }
        }
        inner(&self.0, state);
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

impl Into<Value> for Int {
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

impl Into<Value> for Block {
    fn into(self) -> Value {
        Value::Block(self)
    }
}

pub trait FromValue: Sized {
    fn description() -> &'static str;
    fn matches(value: &Value) -> bool;
    fn from_value(value: Value) -> Option<Self>;
}

impl FromValue for Char {
    fn description() -> &'static str { "character" }

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

impl FromValue for Int {
    fn description() -> &'static str { "integer" }

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
    fn description() -> &'static str { "float" }

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
    fn description() -> &'static str { "array" }

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
    fn description() -> &'static str { "block" }

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
    fn description() -> &'static str { "any value" }

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
    fn description() -> &'static str { "char or number" }

    fn matches(v: &Value) -> bool {
        matches!(v, Value::Char(_) | Value::Int(_) | Value::Real(_))
    }

    fn from_value(v: Value) -> Option<Self> {
        Some(v).filter(Self::matches).map(Self)
    }
}

/// Matches any scalar type and converts it to an integer.
pub struct ScalarToInt(pub Option<Int>);

impl FromValue for ScalarToInt {
    fn description() -> &'static str { "char or number" }

    fn matches(v: &Value) -> bool {
        matches!(v, Value::Char(_) | Value::Int(_) | Value::Real(_))
    }

    fn from_value(v: Value) -> Option<Self> {
        match v {
            Value::Char(c) => Some(Self(Some(Int::from(c.0)))),
            Value::Int(i) => Some(Self(Some(i))),
            Value::Real(x) => Some(Self(if x.is_finite() {
                Some(Int::from(x as i64))
            } else {
                None
            })),
            _ => None,
        }
    }
}

/// Matches any numeric type and converts it to a float.
pub struct NumToReal(pub f64);

impl FromValue for NumToReal {
    fn description() -> &'static str { "number" }

    fn matches(v: &Value) -> bool {
        matches!(v, Value::Int(_) | Value::Real(_))
    }

    fn from_value(v: Value) -> Option<Self> {
        match v {
            Value::Int(i) => i.to_f64().map(Self), // this cannot fail
            Value::Real(x) => Some(Self(x)),
            _ => None,
        }
    }
}

/// Matches any numeric type and converts it to an integer.
pub struct NumToInt(pub Option<Int>);

impl FromValue for NumToInt {
    fn description() -> &'static str { "number" }

    fn matches(v: &Value) -> bool {
        matches!(v, Value::Int(_) | Value::Real(_))
    }

    fn from_value(v: Value) -> Option<Self> {
        match v {
            Value::Int(i) => Some(Self(Some(i))),
            Value::Real(x) => Some(Self(if x.is_finite() {
                Some(Int::from(x as i64))
            } else {
                None
            })),
            _ => None,
        }
    }
}

/// Matches an integer or character and converts it to a character.
pub struct IntegralToChar(pub Char);

impl FromValue for IntegralToChar {
    fn description() -> &'static str { "char or integer" }

    fn matches(v: &Value) -> bool {
        matches!(v, Value::Char(_) | Value::Int(_))
    }

    fn from_value(v: Value) -> Option<Self> {
        match v {
            Value::Char(c) => Some(Self(c)),
            Value::Int(i) => Some(Self(Char(bigint_to_u32_wrapping(&i)))),
            _ => None,
        }
    }
}

/// Attempt to downcast a pair of values to the provided types.
pub fn try_downcast_2<T: FromValue, U: FromValue>(a: Value, b: Value)
    -> Result<(T, U), (Value, Value)>
{
    if T::matches(&a) && U::matches(&b) {
        Ok(T::from_value(a).zip(U::from_value(b))
            .expect("invalid `matches()` method"))
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
