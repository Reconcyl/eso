use std::collections::HashMap;

use super::parse::{Ins, Dir, InscriptionIdx};

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
#[derive(Hash, PartialEq, Eq)]
pub struct Pos { pub x: isize, pub y: isize }

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
pub struct World {
    regions: HashMap<Pos, Region>,
    inscriptions: HashMap<Pos, InscriptionIdx>,
    miner: Pos,
    program: Vec<Vec<Ins>>,
    stack: Vec<StackFrame>,
    current_frame: StackFrame,
}

/// Represents a call stack frame.
struct StackFrame {
    ins_idx: InscriptionIdx,
    pos: usize,
}

impl World {
    pub fn new(program: Vec<Vec<Ins>>) -> Self {
        assert!(!program.is_empty());
        Self {
            regions: HashMap::new(),
            inscriptions: HashMap::new(),
            miner: Pos { x: 0, y: 0 },
            stack: Vec::new(),
            current_frame: StackFrame {
                ins_idx: InscriptionIdx(program.len() - 1),
                pos: 0,
            },
            program,
        }
    }

    pub fn get(&self, pos: Pos) -> bool {
        let (reg_id, idx) = pos.region();
        let region = match self.regions.get(&reg_id) {
            Some(r) => r,
            None => return false
        };
        let byte_idx = idx / 8;
        let bit_idx = idx % 8;
        (region.0[byte_idx] >> bit_idx & 1) != 0
    }

    pub fn set(&mut self, pos: Pos, new: bool) {
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
}
