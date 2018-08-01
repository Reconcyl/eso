mod stack;
mod parse;

use self::stack::{Datum, Stack};
use self::parse::{FunctionArena, FunctionIndex};

#[derive(Debug, Clone)]
enum CommandType {
    One,
    Zero,
    Function(FunctionIndex),
    Invert,
    Or,
    And,
    Dup,
    Swap,
    Delete,
    ShiftUp,
    ShiftDown,
    Return,
    Compare,
    Exec,
    CondExec,
    Other(char),
}

struct Command {
    line: usize,
    col: usize,
    type_: CommandType
}

type Subprogram = Vec<Command>;

struct Position {
    line: usize,
    col: usize,
}

struct ErrorData {
    // The call stack is stored deepest-first, because each layer successively appends to it as
    call_stack: Vec<(FunctionIndex, Position)>,
    msg: String
}

struct State {
    functions: FunctionArena,
    stack: Stack
}

impl ErrorData {
    fn with_msg<S: Into<String>>(msg: S) -> Self {
        Self {
            call_stack: Vec::new(),
            msg: msg.into()
        }
    }
    fn add_layer(&mut self, idx: FunctionIndex, pos: Position) {
        self.call_stack.push((idx, pos));
    }
    fn add_layer_fn(idx: FunctionIndex, pos: Position) -> impl FnOnce(Self) -> Self {
        |s| {
            s.add_layer(idx, pos);
            s
        }
    }
}

impl State {
    fn new(functions: FunctionArena) -> Self {
        Self {
            functions,
            stack: Stack::new()
        }
    }
    fn pop_or_err(&mut self) -> Result<Datum, ErrorData> {
        self.stack.pop().ok_or_else(|| ErrorData::with_msg("the stack is empty"))
    }
    fn pop_bit_or_err(&mut self) -> Result<bool, ErrorData> {
        match self.pop_or_err()? {
            Datum::Function(_) => Err(ErrorData::with_msg("expected bit, got function")),
            Datum::Bit(b) => Ok(b)
        }
    }
    fn pop_function_or_err(&mut self) -> Result<FunctionIndex, ErrorData> {
        match self.pop_or_err()? {
            Datum::Bit(_) => Err(ErrorData::with_msg("expected function, got bit")),
            Datum::Function(idx) => Ok(idx)
        }
    }
    fn shift_up_or_err(&mut self) -> Result<(), ErrorData> {
        self.stack.shift_up().ok_or_else(|| ErrorData::with_msg("can't shift up any further"))
    }
    fn shift_down_or_err(&mut self) -> Result<(), ErrorData> {
        self.stack.shift_down().ok_or_else(|| ErrorData::with_msg("can't shift down any further"))
    }
    fn run_command(&mut self, c: &Command) -> Result<bool, ErrorData> {
        use self::Command as C;
        match *c {
            C::One => self.stack.push(
                Datum::Bit(true)),
            C::Zero => self.stack.push(
                Datum::Bit(false)),
            C::Function(idx) => self.stack.push(
                Datum::Function(idx)),
            C::Invert => self.stack.push(
                Datum::Bit(!self.pop_bit_or_err()?)),
            C::Or => self.stack.push(
                Datum::Bit(self.pop_bit_or_err()? | self.pop_bit_or_err()?)),
            C::And => self.stack.push(
                Datum::Bit(self.pop_bit_or_err()? & self.pop_bit_or_err()?)),
            C::Dup => {
                let a = self.pop_or_err()?;
                self.stack.push(a.clone());
                self.stack.push(a);
            }
            C::Swap => {
                let (a, b) = (self.pop_or_err()?, self.pop_or_err()?);
                self.stack.push(a);
                self.stack.push(b);
            }
            C::Delete => { self.pop_or_err()?; },
            C::ShiftUp => self.shift_up_or_err()?,
            C::ShiftDown => self.shift_down_or_err()?,
            C::Return => return Ok(false),
            C::Compare => self.stack.push(
                Datum::Bit(self.pop_or_err()? == self.pop_or_err()?)),
            C::Exec => return self.run_function(self.pop_function_or_err()?),
            C::CondExec => {
                let cond = self.pop_bit_or_err()?;
                let idx = self.pop_function_or_err()?;
                if cond {
                    return self.run_function(idx);
                }
            }
            C::Other(c) => return ErrorData::with_msg(
                format!("Unexpected command {:?}", c))
        }
        Ok(true)
    }
    fn run_commands(&mut self, commands: &[Command]) -> Result<bool, (ErrorData, Position)> {
        let mut line = 0;
        let mut col = 0;
        for c in commands {
            if c == Command::Other('\n') {
                col = 0;
                line += 1;
            } else {
                col += 1;
            }
        }
    }
}