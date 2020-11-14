pub struct Bytecode {
    pub bytes: Vec<u8>,
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
    Not    = 0x00,
    Plus   = 0x01,
    One    = 0x02,
    LowerA = 0x03,
});
