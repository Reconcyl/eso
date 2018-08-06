type Bit = bool;

fn nth_bit(byte: u8, bit_index: u32) -> Bit {
    byte.wrapping_shr(7 - bit_index) & 1 == 1
}

struct BitIterator<I: Iterator<Item=u8>> {
    iterator: I,
    current_byte: u8
    current_bit_index: u8
};

impl<I: Iterator<Item=u8>> BitIterator<I> {
    fn from(mut iterator: I) -> Self {
        let current_byte = i.next().unwrap_or(0);
        BitIterator {
            iterator,
            current_byte,
            current_bit_index: 0
        }
    }
}

impl<I: Iterator<Item=u8>> Iterator for BitIterator<I> {
    type Item = Bit;
    fn next(&mut self) -> Option<Self::Item> {
        let result = nth_bit(self.current_byte, self.current_bit_index);
        self.current_bit_index += 1;
        if self.current_bit_index >= 8 {
            self.current_bit_index = 0;
            self.current_byte = self.iterator.next().unwrap_or(0);
        }
        result
    }
}

pub fn from_binary(bytes: &[u8]) -> String {
    unimplemented!()
}