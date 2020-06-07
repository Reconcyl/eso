use std::collections::HashMap;
use std::fmt;
use std::mem;

use super::parse::{Ins, Dir, DirIns, InscriptionIdx};

mod io;
mod debug;

pub use io::{StdIo, BufIo};
pub use debug::Debugger;

/// For memory efficiency, the memory plane is divided up into square-shaped regions.
const REGION_SIZE: usize = 64;

/// A region of the grid.
struct Region([u8; REGION_SIZE * REGION_SIZE / 8]);

impl Default for Region {
    fn default() -> Self {
        Self([0; REGION_SIZE * REGION_SIZE / 8])
    }
}

/// Represents a position on the grid.
#[derive(Clone, Copy, Hash, PartialEq, Eq)]
pub struct Pos { pub x: isize, pub y: isize }

impl fmt::Debug for Pos {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", (self.x, self.y))
    }
}

impl std::ops::Add<Dir> for Pos {
    type Output = Pos;
    fn add(mut self, dir: Dir) -> Pos {
        match dir {
            Dir::N => self.y -= 1,
            Dir::E => self.x += 1,
            Dir::W => self.x -= 1,
            Dir::S => self.y += 1,
        }
        self
    }
}

impl Pos {
    fn region(self) -> (Self, usize) {
        let reg_x = self.x.div_euclid(REGION_SIZE as isize);
        let reg_y = self.y.div_euclid(REGION_SIZE as isize);
        let sub_x = self.x.rem_euclid(REGION_SIZE as isize) as usize;
        let sub_y = self.y.rem_euclid(REGION_SIZE as isize) as usize;
        (Self { x: reg_x, y: reg_y }, sub_y * REGION_SIZE + sub_x)
    }
}

/// The execution state.
pub struct World<Io> {
    regions: HashMap<Pos, Region>,
    inscriptions: HashMap<Pos, InscriptionIdx>,
    miner: Pos,
    program: Vec<Vec<Ins>>,
    stack: Vec<StackFrame>,
    current_frame: StackFrame,
    io: Io,
}

/// Represents a call stack frame.
#[derive(Clone, Copy, Debug)]
struct StackFrame {
    ins_idx: InscriptionIdx,
    pos: usize,
}

impl<Io: io::Io> World<Io> {
    pub fn new(program: Vec<Vec<Ins>>, io: Io) -> Self {
        assert!(!program.is_empty());
        let mut self_ = Self {
            regions: HashMap::new(),
            inscriptions: HashMap::new(),
            miner: Pos { x: 0, y: 0 },
            stack: Vec::new(),
            current_frame: StackFrame {
                ins_idx: InscriptionIdx(program.len() - 1),
                pos: 0,
            },
            program,
            io,
        };
        self_.set(self_.miner, true);
        self_
    }

    fn get(&self, pos: Pos) -> bool {
        let (reg_id, idx) = pos.region();
        let region = match self.regions.get(&reg_id) {
            Some(r) => r,
            None => return false
        };
        let byte_idx = idx / 8;
        let bit_idx = idx % 8;
        (region.0[byte_idx] >> bit_idx & 1) != 0
    }

    fn set(&mut self, pos: Pos, new: bool) {
        if new {
            self.inscriptions.remove(&pos);
        }
        let (reg_id, idx) = pos.region();
        let region = self.regions.entry(reg_id)
            .or_default();
        let byte_idx = idx / 8;
        let bit_idx = idx % 8;
        let mask = 1u8 << bit_idx;
        if new {
            region.0[byte_idx] |= mask;
        } else {
            region.0[byte_idx] &= !mask;
        }
    }

    fn step(&mut self) -> bool {
        let frame = self.current_frame;
        if let Some(&ins) = self.program[frame.ins_idx.0].get(frame.pos) {
            self.current_frame.pos += 1;
            self.run_ins(ins)
        } else if let Some(new_frame) = self.stack.pop() {
            self.current_frame = new_frame;
            true
        } else {
            false
        }
    }

    fn run_ins(&mut self, ins: Ins) -> bool {
        match ins {
            Ins::Dir(dir, dir_ins) => { self.run_dir_ins(dir, dir_ins); true }
            Ins::Nop => true,
            Ins::End => false,
        }
    }

    fn run_dir_ins(&mut self, dir: Dir, ins: DirIns) {
        let target = self.miner + dir;
        match ins {
            DirIns::Dig => {
                self.set(target, true);
                self.inscriptions.remove(&target);
            }
            DirIns::Fill => self.set(target, false),
            DirIns::Step => if self.get(target) {
                self.miner = target;
            }
            DirIns::Walk => while self.get(self.miner + dir) {
                self.miner = self.miner + dir;
            }
            DirIns::Input => {
                let bit = self.io.read();
                self.set(target, bit);
            }
            DirIns::Output => {
                let bit = self.get(target);
                self.io.write(bit);
            }
            DirIns::Execute => {
                if let Some(&ins) = self.inscriptions.get(&target) {
                    self.stack.push(mem::replace(&mut self.current_frame, StackFrame {
                        ins_idx: ins,
                        pos: 0,
                    }))
                }
            }
            DirIns::InscribeMe => {
                if !self.get(target) {
                    self.inscriptions.insert(target, self.current_frame.ins_idx);
                }
            }
            DirIns::Inscribe(idx) => {
                if !self.get(target) {
                    self.inscriptions.insert(target, idx);
                }
            }
        }
    }

    pub fn run(&mut self) {
        while self.step() {}
    }
}
