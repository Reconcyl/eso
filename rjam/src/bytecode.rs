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
    Lit    = 0x00,
    Not    = 0x01,
    Plus   = 0x02,
    One    = 0x03,
    LowerA = 0x04,
});
