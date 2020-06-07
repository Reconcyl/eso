use std::collections::VecDeque;
use std::io::{Bytes, Read, Write};

pub trait Io {
    fn read(&mut self) -> bool;
    fn write(&mut self, bit: bool);
}

pub struct StdIo<R, W> {
    stdin: Bytes<R>,
    in_byte: u8, // the byte most recently read from stdin
    in_bit: u8, // how many bits of this byte have already been read?

    stdout: W,
    out_byte: u8, // the byte being prepared to be written to stdout
    out_bit: u8, // how many bits of this byte have wrtten?
}

impl<R: Read, W: Write> StdIo<R, W> {
    pub fn new(stdin: R, stdout: W) -> Self {
        Self {
            stdin: stdin.bytes(),
            in_byte: 0,
            in_bit: 8,

            stdout,
            out_byte: 0,
            out_bit: 0,
        }
    }
}

impl<R: Read, W: Write> Io for StdIo<R, W> {
    fn read(&mut self) -> bool {
        if self.in_bit == 8 {
            self.in_byte = match self.stdin.next() {
                Some(Ok(b)) => b,
                _ => 0xFF,
            };
            self.in_bit = 0;
        }
        self.in_bit += 1;
        self.in_byte >> (8 - self.in_bit) & 1 != 0
    }

    fn write(&mut self, b: bool) {
        self.out_bit += 1;
        if b {
            self.out_byte |= 1 << (8 - self.out_bit);
        }
        if self.out_bit == 8 {
            self.stdout.write_all(&[self.out_byte]).unwrap();
            self.out_byte = 0;
            self.out_bit = 0;
        }
    }
}

#[derive(Default)]
pub struct BufIo {
    input: VecDeque<bool>,
    output: Vec<bool>,
}

impl Io for BufIo {
    fn read(&mut self) -> bool {
        self.input.pop_front().unwrap_or(true)
    }
    
    fn write(&mut self, bit: bool) {
        self.output.push(bit);
    }
}
