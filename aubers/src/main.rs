use num_bigint::BigInt;
use num_traits::cast::ToPrimitive as _;

mod util;

/// Possible circumstances that can halt the execution
/// of am Aubergine program.
enum Halt {
    UnknownCmd(BigInt),
    UnknownArg(BigInt),
    OutOfBounds(BigInt),
    End,
}

/// The different kinds of commands.
enum Cmd { Mov, Add, Sub, Jnz }

/// One of the two standard registers.
enum Reg { A, B }

/// One of the standard expressions.
enum Arg { VarA, VarB, VarI, VarO, RefA, RefB, One }

/// The state of a running Aubergine program.
struct State {
    a: BigInt,
    b: BigInt,
    ip: usize,
    tape: Vec<BigInt>,
}

impl State {
    /// Initialize the state.
    fn new(code: Vec<BigInt>) -> Self {
        Self {
            a: BigInt::default(),
            b: BigInt::default(),
            ip: 0,
            tape: code,
        }
    }

    /// Get an element from a tape associated with a given index.
    fn deref<'a>(tape: &'a [BigInt], idx: &BigInt) -> Result<&'a BigInt, Halt> {
        if let Some(i) = idx.to_usize() {
            if let Some(v) = tape.get(i) {
                return Ok(v);
            }
        }
        Err(Halt::OutOfBounds(idx.clone()))
    }

    /// Mutably get an element from a tape associated with a given index.
    fn deref_mut<'a>(tape: &'a mut [BigInt], idx: &BigInt)
        -> Result<&'a mut BigInt, Halt>
    {
        if let Some(i) = idx.to_usize() {
            if let Some(v) = tape.get_mut(i) {
                return Ok(v);
            }
        }
        Err(Halt::OutOfBounds(idx.clone()))
    }

    /// Dereference one register and copy it to another register.
    fn mem_to_reg(&mut self, dst: Reg, src: Reg) -> Result<(), Halt> {
        let reg_val = Self::deref(&self.tape, match src {
            Reg::A => &self.a,
            Reg::B => &self.b,
        })?;
        match dst {
            Reg::A => self.a.clone_from(reg_val),
            Reg::B => self.b.clone_from(reg_val),
        }
        Ok(())
    }

    /// Attempt to interpret the value at a given index as a command.
    fn parse_cmd(&self, pos: usize) -> Result<Cmd, Halt> {
        match self.tape.get(pos) {
            Some(i) =>
                match i.to_u8() {
                    Some(b'=') => Ok(Cmd::Mov),
                    Some(b'+') => Ok(Cmd::Add),
                    Some(b'-') => Ok(Cmd::Sub),
                    Some(b':') => Ok(Cmd::Jnz),
                    _ => Err(Halt::UnknownCmd(i.clone())),
                }
            // index out of bounds
            None => Err(Halt::End),
        }
    }

    /// Attempt to interpret the value at a given index as an argument.
    fn parse_arg(&self, pos: usize) -> Result<Arg, Halt> {
        match self.tape.get(pos) {
            Some(i) =>
                match i.to_u8() {
                    Some(b'a') => Ok(Arg::VarA),
                    Some(b'b') => Ok(Arg::VarB),
                    Some(b'i') => Ok(Arg::VarI),
                    Some(b'o') => Ok(Arg::VarO),
                    Some(b'A') => Ok(Arg::RefA),
                    Some(b'B') => Ok(Arg::RefB),
                    Some(b'1') => Ok(Arg::One),
                    _ => Err(Halt::UnknownArg(i.clone())),
                }
            // index out of bounds
            None => Err(Halt::End),
        }
    }

    /// Execute a single instruction.
    fn step(&mut self) -> Result<(), Halt> {
        // there's no way `tape` is large enough for this to overflow
        let cmd = self.parse_cmd(self.ip)?;
        let arg_1 = self.parse_arg(self.ip + 1)?;
        let arg_2 = self.parse_arg(self.ip + 2)?;

        use Cmd::*;
        use Arg::*;
        match (cmd, arg_1, arg_2) {
            // redundant assignments
            (Mov, VarA, VarA) => {}
            (Mov, RefA, RefA) => {}
            (Mov, VarB, VarB) => {}
            (Mov, RefB, RefB) => {}

            // register-to-register assignments
            (Mov, VarA, VarB) => self.a.clone_from(&self.b),
            (Mov, VarB, VarA) => self.b.clone_from(&self.a),

            // memory-to-register assignments
            (Mov, VarA, RefA) => self.mem_to_reg(Reg::A, Reg::A)?,
            (Mov, VarA, RefB) => self.mem_to_reg(Reg::A, Reg::B)?,
            (Mov, VarB, RefA) => self.mem_to_reg(Reg::B, Reg::A)?,
            (Mov, VarB, RefB) => self.mem_to_reg(Reg::B, Reg::B)?,

            // const assignments
            (Mov, VarA, One) => util::assign_from_u8(&mut self.a, 1),
            (Mov, VarB, One) => util::assign_from_u8(&mut self.b, 1),

            (Add, _, _) | (Sub, _, _) | (Jnz, _, _) => todo!(),
            _ => todo!(),
        }
        todo!()
    }
}

fn main() {
    let mut state = State::new(Vec::new());
    let _ = state.step();
    println!("Hello, world!");
}
