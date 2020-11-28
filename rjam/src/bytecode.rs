use crate::value::Value;

#[derive(Clone)]
pub struct Bytecode {
    pub bytes: Vec<u8>,
    pub consts: Vec<Value>,
}

impl Bytecode {
    pub fn new() -> Self {
        Self {
            bytes: Vec::new(),
            consts: Vec::new(),
        }
    }
}

macro_rules! opcodes {
    ($v:vis enum $name:ident {
        $($variant:ident $(= $val:expr)?,)*
    }) => {
        #[repr(u8)]
        $v enum $name {
            $($variant $(= $val)?,)*
        }

        impl $name {
            pub fn from_byte(byte: u8) -> Option<Self> {
                match byte {
                    $(b if b == Self::$variant as u8
                        => Some(Self::$variant),)*
                    _ => None
                }
            }
        }
    }
}

opcodes!(pub enum Opcode {
    // Miscellaneous/nullary opreators
    Lit = 0x00,
    LeftBracket,
    RightBracket,

    // Unary operators
    Excl = 0x40,
    Dollar,
    LeftParen,
    RightParen,
    Comma,
    Underscore,
    LowerA,

    // Binary operators
    Hash = 0x80,
    Percent,
    And,
    Star,
    Plus,
    Minus,
});

/// The syntactic arity of an operator. Note that some operators
/// (e.g. `$`) have a different arity depending on the TOS, so
/// this may not reflect the number of values popped at runtime.
pub enum Arity {
    Misc,
    Unary,
    Binary,
}

impl Opcode {
    pub fn arity(self) -> Arity {
        match self as u8 {
            0x00 ..= 0x3f => Arity::Misc,
            0x40 ..= 0x7f => Arity::Unary,
            0x80 ..= 0xbf => Arity::Binary,
            _ => unreachable!(),
        }
    }
}
