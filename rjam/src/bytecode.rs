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
    Lit    = 0x00,
    One    = 0x01,

    // Unary operators
    Not    = 0x40,
    LowerA = 0x41,

    // Binary operators
    Hash   = 0x80,
    Plus   = 0x81,
});
