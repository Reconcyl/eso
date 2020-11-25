use crate::value::Value;

#[derive(Clone)]
pub struct Bytecode {
    pub bytes: Vec<u8>,
    pub consts: Vec<Value>,
}

macro_rules! opcodes {
    ($v:vis $name:ident {
        $($variant:ident = $val:expr,)*
    }) => {
        #[repr(u8)]
        $v enum $name {
            $($variant = $val,)*
        }

        impl $name {
            pub fn from_byte(byte: u8) -> Option<Self> {
                match byte {
                    $($val => Some(Self::$variant),)*
                    _ => None
                }
            }
        }
    }
}

opcodes!(pub Opcode {
    // Miscellaneous/nullary opreators
    Lit          = 0x00,
    LeftBracket  = 0x01,
    RightBracket = 0x02,

    // Unary operators
    Excl       = 0x40,
    Dollar     = 0x41,
    LeftParen  = 0x42,
    RightParen = 0x43,
    Underscore = 0x44,
    LowerA     = 0x45,

    // Binary operators
    Hash    = 0x80,
    Percent = 0x81,
    And     = 0x82,
    Star    = 0x83,
    Plus    = 0x84,
});
