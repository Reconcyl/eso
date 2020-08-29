use num_bigint::BigInt;
use num_traits::cast::ToPrimitive as _;
use num_traits::Zero as _;

use std::io::{self, BufReader, Read, Write};
use std::mem;

mod util;
use util::Option2;

/// Possible circumstances that can halt the execution
/// of am Aubergine program.
enum Halt {
    UnknownCmd(BigInt),
    UnknownArg(BigInt),
    OutOfBounds(BigInt),
    OutputNotByte(BigInt),
    AssignToOne,
    VarOLogic,
    NoProgramFile,
    Io(io::Error),
    End,
}

/// The different kinds of commands.
enum Cmd { Mov, Add, Sub, Jnz }

/// One of the two standard registers.
enum Reg { A, B }

/// One of the standard expressions.
enum Arg { VarA, VarB, VarI, VarO, RefA, RefB, One }

/// The result of evaluating an argument.
#[derive(Clone)]
enum EvalResult<'a> {
    Owned(BigInt),
    Large(&'a BigInt),
    Small(usize),
}

impl EvalResult<'_> {
    fn to_bigint(&self) -> BigInt {
        match *self {
            Self::Owned(ref n) => n.clone(),
            Self::Large(n) => n.clone(),
            Self::Small(n) => n.into(),
        }
    }

    fn to_usize(&self) -> Option<usize> {
        match *self {
            Self::Owned(ref n) => n.to_usize(),
            Self::Large(n) => n.to_usize(),
            Self::Small(n) => Some(n),
        }
    }

    fn to_u8(&self) -> Option<u8> {
        self.to_usize()?.to_u8()
    }
}

/// The state of a running Aubergine program.
struct State<I, O> {
    a: BigInt,
    b: BigInt,
    ip: usize,
    tape: Vec<BigInt>,

    input: io::Bytes<I>,
    output: O,
}

impl<I: Read, O: Write> State<I, O> {
    /// Initialize the state.
    fn new(code: Vec<BigInt>, input: I, output: O) -> Self {
        Self {
            a: BigInt::default(),
            b: BigInt::default(),
            ip: 0,
            tape: code,
            input: input.bytes(),
            output,
        }
    }

    /// Read a single byte from input.
    fn read(&mut self) -> Result<BigInt, Halt> {
        match self.input.next() {
            Some(Err(e)) => Err(Halt::Io(e)),
            Some(Ok(b)) => Ok(BigInt::from(b)),
            None => Ok(BigInt::from(-1)),
        }
    }

    /// Write a single byte to output.
    fn write(&mut self, b: u8) -> Result<(), Halt> {
        self.output.write_all(&[b]).map_err(Halt::Io)
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

    /// Get mutable references to the values referred to by each register.
    /// Return `None` if the registers are equal or `Err(_)` if they are
    /// out of bounds.
    fn deref_regs_mut(&mut self)
        -> Result<Option2<&mut BigInt>, Halt>
    {
        let idx_a = match self.a.to_usize() {
            Some(a) => a,
            None => return Err(Halt::OutOfBounds(self.a.clone())),
        };
        let idx_b = match self.b.to_usize() {
            Some(b) => b,
            None => return Err(Halt::OutOfBounds(self.b.clone())),
        };

        let ref_a = self.tape.get_mut(idx_a)
            .ok_or(Halt::OutOfBounds(self.a.clone()))?
            as *mut BigInt;
        let ref_b = self.tape.get_mut(idx_b)
            .ok_or(Halt::OutOfBounds(self.b.clone()))?;

        Ok(if std::ptr::eq(ref_a, ref_b) {
            Option2::One(ref_b)
        } else {
            // SAFETY: we have already checked that `ref_a != ref_b`.
            unsafe {
                Option2::Two(
                    ref_a.as_mut().unwrap(),
                    ref_b,
                )
            }
        })
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

    /// Add the value of one register to the dereferenced value
    /// of another register.
    fn reg_to_mem_add(&mut self, dst: Reg, src: Reg) -> Result<(), Halt> {
        let reg_val = Self::get_mut(&mut self.tape, match dst {
            Reg::A => &self.a,
            Reg::B => &self.b,
        })?;
        match src {
            Reg::A => *reg_val += &self.a,
            Reg::B => *reg_val += &self.b,
        }
        Ok(())
    }

    /// Subtract the value of one register from the dereferenced value
    /// of another register.
    fn reg_to_mem_sub(&mut self, dst: Reg, src: Reg) -> Result<(), Halt> {
        let reg_val = Self::get_mut(&mut self.tape, match dst {
            Reg::A => &self.a,
            Reg::B => &self.b,
        })?;
        match src {
            Reg::A => *reg_val -= &self.a,
            Reg::B => *reg_val -= &self.b,
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
            VarO => Ok(f(Owned(self.read()?))),
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

            // memory-to-memory assignments
            (Mov, RefA, RefB) =>
                if let Option2::Two(ref_a, ref_b) = self.deref_regs_mut()? {
                    ref_a.clone_from(ref_b);
                }
            (Mov, RefB, RefA) =>
                if let Option2::Two(ref_a, ref_b) = self.deref_regs_mut()? {
                    ref_b.clone_from(ref_a);
                }

            // memory-to-register assignments
            (Mov, VarA, RefA) => self.mem_to_reg(Reg::A, Reg::A)?,
            (Mov, VarA, RefB) => self.mem_to_reg(Reg::A, Reg::B)?,
            (Mov, VarB, RefA) => self.mem_to_reg(Reg::B, Reg::A)?,
            (Mov, VarB, RefB) => self.mem_to_reg(Reg::B, Reg::B)?,

            // register-to-memory assignments
            (Mov, RefA, VarA) => {
                let ref_a = Self::get_mut(&mut self.tape, &self.a)?;
                ref_a.clone_from(&self.a);
            }
            (Mov, RefA, VarB) => {
                let ref_a = Self::get_mut(&mut self.tape, &self.a)?;
                ref_a.clone_from(&self.b);
            }
            (Mov, RefA, VarI) => {
                let ref_a = Self::get_mut(&mut self.tape, &self.a)?;
                util::assign_from_usize(ref_a, self.ip)
            }
            (Mov, RefB, VarA) => {
                let ref_b = Self::get_mut(&mut self.tape, &self.b)?;
                ref_b.clone_from(&self.a);
            }
            (Mov, RefB, VarB) => {
                let ref_b = Self::get_mut(&mut self.tape, &self.b)?;
                ref_b.clone_from(&self.b);
            }
            (Mov, RefB, VarI) => {
                let ref_b = Self::get_mut(&mut self.tape, &self.b)?;
                util::assign_from_usize(ref_b, self.ip)
            }

            // const assignments
            (Mov, VarA, One) => util::assign_from_u8(&mut self.a, 1),
            (Mov, VarB, One) => util::assign_from_u8(&mut self.b, 1),
            (Mov, RefA, One) => {
                let ref_a = Self::get_mut(&mut self.tape, &self.a)?;
                util::assign_from_u8(ref_a, 1);
            }
            (Mov, RefB, One) => {
                let ref_b = Self::get_mut(&mut self.tape, &self.b)?;
                util::assign_from_u8(ref_b, 1);
            }

            // assignments to `i`
            (Mov, VarI, arg) =>
                self.ip = self.eval(arg, |r| r.to_usize().ok_or(Halt::End))??,

            // assignments to `o`
            (Mov, VarO, arg) => {
                let b = self.eval(arg, |r| match r.to_u8() {
                    Some(b) => Ok(b),
                    None => Err(Halt::OutputNotByte(r.to_bigint())),
                })??;
                self.write(b)?
            }

            // assigning `i` to things
            (Mov, VarA, VarI) => util::assign_from_usize(&mut self.a, self.ip),
            (Mov, VarB, VarI) => util::assign_from_usize(&mut self.b, self.ip),

            // assigning `o` to things
            (Mov, VarA, VarO) => {
                let b = self.read()?;
                self.a = b;
            }
            (Mov, VarB, VarO) => {
                let b = self.read()?;
                self.b = b;
            }
            (Mov, RefA, VarO) => {
                let b = self.read()?;
                let ref_a = Self::get_mut(&mut self.tape, &self.a)?;
                *ref_a = b;
            }
            (Mov, RefB, VarO) => {
                let b = self.read()?;
                let ref_b = Self::get_mut(&mut self.tape, &self.b)?;
                *ref_b = b;
            }

            // `o` is not allowed outside of assignments
            (_, VarO, _) | (_, _, VarO) => return Err(Halt::VarOLogic),

            // assignments to `1` are not allowed
            (_, One, _) => return Err(Halt::AssignToOne),

            // doubling values
            (Add, VarA, VarA) => self.a *= 2,
            (Add, VarB, VarB) => self.b *= 2,
            (Add, VarI, VarI) => self.ip *= 2, // should never overflow
            (Add, RefA, RefA) => {
                let ref_a = Self::get_mut(&mut self.tape, &self.a)?;
                *ref_a *= 2;
            }
            (Add, RefB, RefB) => {
                let ref_b = Self::get_mut(&mut self.tape, &self.b)?;
                *ref_b *= 2;
            }

            // adding things to `a` and `b`
            (Add, VarA, arg) => {
                let mut a = mem::take(&mut self.a);
                self.eval(arg, |r| match r {
                    EvalResult::Owned(n) => a += n,
                    EvalResult::Large(n) => a += n,
                    EvalResult::Small(n) => a += n,
                })?;
                self.a = a;
            }
            (Add, VarB, arg) => {
                let mut b = mem::take(&mut self.b);
                self.eval(arg, |r| match r {
                    EvalResult::Owned(n) => b += n,
                    EvalResult::Large(n) => b += n,
                    EvalResult::Small(n) => b += n,
                })?;
                self.b = b;
            }

            // memory-to-memory addition
            (Add, RefA, RefB) =>
                match self.deref_regs_mut()? {
                    Option2::One(ref_ab) => *ref_ab += 1,
                    Option2::Two(ref_a, ref_b) => *ref_a += &*ref_b,
                }
            (Add, RefB, RefA) =>
                match self.deref_regs_mut()? {
                    Option2::One(ref_ab) => *ref_ab += 1,
                    Option2::Two(ref_a, ref_b) => *ref_b += &*ref_a,
                }

            // register-to-memory addition
            (Add, RefA, VarA) => self.reg_to_mem_add(Reg::A, Reg::A)?,
            (Add, RefA, VarB) => self.reg_to_mem_add(Reg::A, Reg::B)?,
            (Add, RefB, VarA) => self.reg_to_mem_add(Reg::B, Reg::A)?,
            (Add, RefB, VarB) => self.reg_to_mem_add(Reg::B, Reg::B)?,

            // adding `1` to things
            (Add, VarI, One) => self.ip += 1,
            (Add, RefA, One) => {
                let ref_a = Self::get_mut(&mut self.tape, &self.a)?;
                *ref_a += 1;
            }
            (Add, RefB, One) => {
                let ref_b = Self::get_mut(&mut self.tape, &self.b)?;
                *ref_b += 1;
            }

            // adding `i` to things
            (Add, RefA, VarI) => {
                let ref_a = Self::get_mut(&mut self.tape, &self.a)?;
                *ref_a += self.ip;
            }
            (Add, RefB, VarI) => {
                let ref_b = Self::get_mut(&mut self.tape, &self.b)?;
                *ref_b += self.ip;
            }

            // adding things to `i`
            (Add, VarI, arg) => {
                // not very efficient, but it's simplest
                // way to handle negatives properly
                let mut new_ip = BigInt::from(self.ip);
                self.eval(arg, |r| match r {
                    EvalResult::Owned(n) => new_ip += n,
                    EvalResult::Large(n) => new_ip += n,
                    EvalResult::Small(n) => new_ip += n,
                })?;
                match new_ip.to_usize() {
                    Some(new_ip) => self.ip = new_ip,
                    None => return Err(Halt::OutOfBounds(new_ip))
                }
            }

            // deleting values
            (Sub, VarA, VarA) => self.a.set_zero(),
            (Sub, VarB, VarB) => self.b.set_zero(),
            (Sub, VarI, VarI) => self.ip = 0,
            (Sub, RefA, RefA) =>
                Self::get_mut(&mut self.tape, &self.a)?.set_zero(),
            (Sub, RefB, RefB) =>
                Self::get_mut(&mut self.tape, &self.b)?.set_zero(),

            // subtracting things from `a` and `b`
            (Sub, VarA, arg) => {
                let mut a = mem::take(&mut self.a);
                self.eval(arg, |r| match r {
                    EvalResult::Owned(n) => a -= n,
                    EvalResult::Large(n) => a -= n,
                    EvalResult::Small(n) => a -= n,
                })?;
                self.a = a;
            }
            (Sub, VarB, arg) => {
                let mut b = mem::take(&mut self.b);
                self.eval(arg, |r| match r {
                    EvalResult::Owned(n) => b -= n,
                    EvalResult::Large(n) => b -= n,
                    EvalResult::Small(n) => b -= n,
                })?;
                self.b = b;
            }

            // memory-to-memory subtraction
            (Sub, RefA, RefB) =>
                match self.deref_regs_mut()? {
                    Option2::One(ref_ab) => ref_ab.set_zero(),
                    Option2::Two(ref_a, ref_b) => *ref_a -= &*ref_b,
                }
            (Sub, RefB, RefA) =>
                match self.deref_regs_mut()? {
                    Option2::One(ref_ab) => ref_ab.set_zero(),
                    Option2::Two(ref_a, ref_b) => *ref_b -= &*ref_a,
                }

            // register-to-memory subtraction
            (Sub, RefA, VarA) => self.reg_to_mem_sub(Reg::A, Reg::A)?,
            (Sub, RefA, VarB) => self.reg_to_mem_sub(Reg::A, Reg::B)?,
            (Sub, RefB, VarA) => self.reg_to_mem_sub(Reg::B, Reg::A)?,
            (Sub, RefB, VarB) => self.reg_to_mem_sub(Reg::B, Reg::B)?,

            // subtracting `1` from things
            (Sub, VarI, One) => match self.ip.checked_sub(1) {
                Some(ip) => self.ip = ip,
                None => return Err(Halt::OutOfBounds(
                    BigInt::from(self.ip) - 1))
            }
            (Sub, RefA, One) => {
                let ref_a = Self::get_mut(&mut self.tape, &self.a)?;
                *ref_a -= 1;
            }
            (Sub, RefB, One) => {
                let ref_b = Self::get_mut(&mut self.tape, &self.b)?;
                *ref_b -= 1;
            }

            // subtracting `i` from things
            (Sub, RefA, VarI) => {
                let ref_a = Self::get_mut(&mut self.tape, &self.a)?;
                *ref_a -= self.ip;
            }
            (Sub, RefB, VarI) => {
                let ref_b = Self::get_mut(&mut self.tape, &self.b)?;
                *ref_b -= self.ip;
            }

            // subtracting things from `i`
            (Sub, VarI, arg) => {
                // not very efficient, but it's the simplest
                // way to handle negatives properly
                let mut new_ip = BigInt::from(self.ip);
                self.eval(arg, |r| match r {
                    EvalResult::Owned(n) => new_ip -= n,
                    EvalResult::Large(n) => new_ip -= n,
                    EvalResult::Small(n) => new_ip -= n,
                })?;
                match new_ip.to_usize() {
                    Some(new_ip) => self.ip = new_ip,
                    None => return Err(Halt::OutOfBounds(new_ip)),
                }
            }

            // conditional jumps
            (Jnz, arg1, arg2) => {
                let do_jump = self.eval(arg2, |r| match r {
                    EvalResult::Owned(n) => !n.is_zero(),
                    EvalResult::Large(n) => !n.is_zero(),
                    EvalResult::Small(n) => n != 0,
                })?;
                if do_jump {
                    let new_ip = self.eval(arg1, |r| match r.to_usize() {
                        Some(new_ip) => Ok(new_ip),
                        None => Err(Halt::OutOfBounds(r.to_bigint())),
                    })??;
                    self.ip = new_ip;
                }
            }
        }

        self.ip += 3;
        Ok(())
    }
}

fn run() -> Result<(), Halt> {
    let args: Vec<_> = std::env::args().skip(1).collect();
    let code: Vec<BigInt> =
        if args.len() == 1 {
            use std::fs::File;
            let file = File::open(&args[0]).map_err(Halt::Io)?;
            BufReader::new(file).bytes()
                .map(|b| Ok(BigInt::from(b?)))
                .collect::<Result<_, _>>()
                .map_err(Halt::Io)?
        } else if args.len() == 2 && args[0] == "-c" {
            args[1].bytes()
                .map(BigInt::from)
                .collect()
        } else {
            return Err(Halt::NoProgramFile)
        };

    let stdin = io::stdin();
    let stdin = stdin.lock();
    let stdout = io::stdout();
    let stdout = stdout.lock();

    let mut state = State::new(code, stdin, stdout);
    // halts using an error
    loop { state.step()? }
}

fn main() {
    if let Err(e) = run() {
        std::process::exit(match e {
            Halt::UnknownCmd(i) => {
                match i.to_u8() {
                    Some(b) => eprintln!("error: byte is not a valid argument: '{}'", b),
                    None => eprintln!("error: integer is not a valid argument: {}", i),
                }
                2
            }
            Halt::UnknownArg(i) => {
                match i.to_u8() {
                    Some(b) => eprintln!("error: byte is not a valid argument: '{}'", b),
                    None => eprintln!("error: integer is not a valid argument: {}", i),
                }
                2
            }
            Halt::OutOfBounds(i) => {
                eprintln!("error: index out of bounds: {}", i);
                2
            }
            Halt::OutputNotByte(i) => {
                eprintln!("io error: value cannot be output \
                           as it does not fit in a byte: {}", i);
                2
            }
            Halt::AssignToOne => {
                eprintln!("error: `1` is not an lvalue");
                2
            }
            Halt::VarOLogic => {
                eprintln!("error: `o` cannot be used outside of assignments");
                2
            }
            Halt::NoProgramFile => {
                eprintln!("io error: please pass a program file");
                1
            }
            Halt::Io(e) => {
                eprintln!("io error: {}", e);
                1
            }
            Halt::End => 0
        })
    }
}
