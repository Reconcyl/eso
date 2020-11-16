use crate::value::Value;

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
    Lit = 0x00,
    One = 0x01,

    // Unary operators
    Excl   = 0x40,
    Dollar = 0x41,
    LowerA = 0x42,

    // Binary operators
    Hash    = 0x80,
    Percent = 0x81,
    Plus    = 0x82,
});
