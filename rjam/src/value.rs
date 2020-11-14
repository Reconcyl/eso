use std::rc::Rc;

use crate::bytecode::Bytecode;

#[derive(Debug)]
pub enum Value {
    Char(char),
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
}

impl Into<Value> for i64 {
    fn into(self) -> Value {
        Value::Int(self)
    }
}

pub trait FromValue<'a>: Sized {
    fn from_value(value: &'a Value) -> Option<Self>;
}

impl FromValue<'_> for i64 {
    fn from_value(v: &Value) -> Option<Self> {
        match *v {
            Value::Int(i) => Some(i),
            _ => None,
        }
    }
}
