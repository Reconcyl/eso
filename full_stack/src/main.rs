use std::collections::VecDeque;
use std::{env, fs};
use std::io::{self, Read, Write};

mod front_end;
use front_end::{Error, Instr};

fn main() {
    let mut args = env::args();
    let prog_name = match args.next() {
        Some(n) => n,
        None => {
            eprintln!("error: no program name - should be impossible");
            return
        }
    };
    let program = match args.next() {
        Some(a) =>
            if a == "-c" {
                // read a program from arguments directly
                match args.next() {
                    Some(p) => p.into_bytes(),
                    None => {
                        eprintln!("error: not enough arguments");
                        help(prog_name);
                        return
                    }
                }
            } else {
                // interpret the argument as a file name
                match fs::read(a) {
                    Ok(p) => p,
                    Err(e) => {
                        eprintln!("error: {}", e);
                        return
                    }
                }
            }
        None => {
            eprintln!("error: not enough arguments");
            help(prog_name);
            return
        }
    };
    if let Some(_) = args.next() {
        eprintln!("error: too many arguments");
        help(prog_name);
        return
    }
    run(program);
}

fn help(prog_name: String) {
    eprintln!("\
usage:
  {0} [filename] - run code stored at [filename]
  {0} -c [code]  - run [code]\
    ", prog_name)
}

fn run(program: Vec<u8>) {
    let stdin = io::stdin();
    let stdin = stdin.lock();

    let stdout = io::stdout();
    let stdout = stdout.lock();

    let mut state = State::new(stdin, stdout, program.into());
    if let Err(e) = state.run() {
        eprintln!("error: {}", e);
    }
}

type Queue = VecDeque<u8>;

struct State<R, W> {
    input: R,
    output: W,
    vars: Variables,
    queue: Queue,
}

impl<R: Read, W: Write> State<R, W> {
    /// Initialize the state.
    fn new(input: R, output: W, code: Queue) -> Self {
        Self {
            input,
            output,
            vars: Variables::new(),
            queue: code,
        }
    }

    /// Read a single byte of input and push it to the queue.
    fn input(&mut self) -> Result<(), Error<Queue>> {
        let mut buf = [0u8];
        if self.input.read(&mut buf).map_err(Error::Io)? == 1 {
            self.queue.push_back(buf[0]);
        } else {
            // set a flag to mark EOF
            let var = b'e';
            self.vars.set(var - b'a', 0);
        }
        Ok(())
    }

    /// Write a single byte to output.
    fn output(&mut self, b: u8) -> Result<(), Error<Queue>> {
        self.output.write_all(&[b]).map_err(Error::Io)
    }

    /// Check the condition of an if statement or while loop.
    fn condition(&mut self, zero: bool) -> bool {
        if zero {
            self.queue.pop_back().unwrap_or(0) != 0
        } else {
            !self.queue.is_empty()
        }
    }

    /// Run a Front End instruction.
    fn run_instr(&mut self, instr: &Instr) {
        match *instr {
            Instr::Add(n) =>
                if let Some(tos) = self.queue.back_mut() {
                    *tos = tos.wrapping_add(n);
                }
            Instr::Sub(n) =>
                if let Some(tos) = self.queue.back_mut() {
                    *tos = tos.wrapping_sub(n);
                }
            Instr::Dup(n) => {
                let end_idx = self.queue.len();
                let start_idx = end_idx.saturating_sub(n as usize);
                self.queue.reserve(n as usize);
                for i in start_idx..end_idx {
                    self.queue.push_back(self.queue[i]);
                }
            }
            Instr::Del(n) => {
                let new_len = self.queue.len().saturating_sub(n as usize);
                self.queue.truncate(new_len);
            }
            Instr::Swap => {
                let len = self.queue.len();
                if len >= 2 {
                    let tmp = self.queue[len - 1];
                    self.queue[len - 1] = self.queue[len - 2];
                    self.queue[len - 2] = tmp;
                }
            }
            Instr::Get(v) =>
                self.queue.push_back(self.vars.get(v)),
            Instr::Set(v) =>
                if let Some(tos) = self.queue.pop_back() {
                    self.vars.set(v, tos);
                }
            Instr::Lit(ref bytes) =>
                self.queue.extend(bytes),
            Instr::Block { zero, repeat, ref content } =>
                if repeat {
                    while self.condition(zero) {
                        self.run_instrs(content);
                    }
                } else {
                    if self.condition(zero) {
                        self.run_instrs(content);
                    }
                }
        }
    }

    /// Run a list of Front End instructions.
    fn run_instrs(&mut self, instrs: &[Instr]) {
        for instr in instrs {
            self.run_instr(instr);
        }
    }

    /// Pop and run instructions until the queue is empty.
    fn run(&mut self) -> Result<(), Error<Queue>> {
        let mut instr_buf = Vec::new();
        while let Some(b) = self.queue.pop_front() {
            match b {
                b'[' => self.input()?,
                b']' =>
                    if let Some(b) = self.queue.pop_front() {
                        self.output(b)?
                    }
                b'<' => {
                    instr_buf.clear();
                    let stream = &mut self.queue;
                    while let Some(instr) = front_end::decode_instr(stream, true)? {
                        instr_buf.push(instr);
                    }
                    self.run_instrs(&instr_buf);
                }
                b => self.output(b)?,
            }
        }
        Ok(())
    }
}

type Var = u8;
struct Variables([u8; 26]);

impl Variables {
    fn new() -> Self {
        Self([0; 26])
    }

    fn get(&self, v: Var) -> u8 {
        self.0[v as usize]
    }

    fn set(&mut self, v: Var, new: u8) {
        self.0[v as usize] = new;
    }
}
