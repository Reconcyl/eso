use num_bigint::BigInt;
use num_traits::cast::ToPrimitive as _;

mod util;

/// Possible circumstances that can halt the execution
/// of am Aubergine program.
enum Halt {
    UnknownCmd(BigInt),
    UnknownArg(BigInt),
    OutOfBounds(BigInt),
    NotByte(BigInt),
    End,
}

/// The different kinds of commands.
enum Cmd { Mov, Add, Sub, Jnz }

/// One of the two standard registers.
enum Reg { A, B }

/// One of the standard expressions.
enum Arg { VarA, VarB, VarI, VarO, RefA, RefB, One }

/// The result of evaluating an argument.
#[derive(Clone, Copy)]
enum EvalResult<'a> {
    Large(&'a BigInt),
    Small(usize),
}

impl EvalResult<'_> {
    fn to_bigint(self) -> BigInt {
        match self {
            Self::Large(n) => n.clone(),
            Self::Small(n) => n.into(),
        }
    }

    fn to_usize(self) -> Option<usize> {
        match self {
            Self::Large(n) => n.to_usize(),
            Self::Small(n) => Some(n),
        }
    }

    fn to_u8(self) -> Option<u8> {
        self.to_usize()?.to_u8()
    }
}

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

    /// Read a single byte from input.
    fn read(&mut self) -> Result<u8, Halt> {
        todo!()
    }

    /// Write a single byte to output.
    fn write(&mut self, _b: u8) -> Result<(), Halt> {
        todo!()
    }

    /// Get an element from a tape associated with a given index.
    fn get<'a>(tape: &'a [BigInt], idx: &BigInt) -> Result<&'a BigInt, Halt> {
        if let Some(i) = idx.to_usize() {
            if let Some(v) = tape.get(i) {
                return Ok(v);
            }
        }
        Err(Halt::OutOfBounds(idx.clone()))
    }

    /// Mutably get an element from a tape associated with a given index.
    fn get_mut<'a>(tape: &'a mut [BigInt], idx: &BigInt)
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
        let reg_val = Self::get(&self.tape, match src {
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

    /// Evaluate an argument and apply a function to the result.
    fn eval<F, R>(&mut self, arg: Arg, f: F) -> Result<R, Halt>
        where F: FnOnce(EvalResult) -> R
    {
        use Arg::*;
        use EvalResult::*;
        match arg {
            VarA => Ok(f(Large(&self.a))),
            VarB => Ok(f(Large(&self.b))),
            VarI => Ok(f(Small(self.ip))),
            VarO => Ok(f(Small(self.read()? as usize))),
            RefA => Ok(f(Large(Self::get(&self.tape, &self.a)?))),
            RefB => Ok(f(Large(Self::get(&self.tape, &self.b)?))),
            One => Ok(f(Small(1))),
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
            (Mov, VarI, VarI) => {}

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

            // assignments to `i`
            (Mov, VarI, arg) =>
                self.ip = self.eval(arg, |r| r.to_usize().ok_or(Halt::End))??,

            // assignments to `o`
            (Mov, VarO, arg) => {
                let b = self.eval(arg, |r| match r.to_u8() {
                    Some(b) => Ok(b),
                    None => Err(Halt::NotByte(r.to_bigint())),
                })??;
                self.write(b)?
            }

            (Add, _, _) | (Sub, _, _) | (Jnz, _, _) => todo!(),
            _ => todo!(),
        }
        Ok(())
    }
}

fn main() {
    let mut state = State::new(Vec::new());
    let _ = state.step();
    println!("Hello, world!");
}
