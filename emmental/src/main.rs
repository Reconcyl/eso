use std::collections::VecDeque;
use std::rc::Rc;

use std::io;
use std::path::Path;

type Symbol = u8;
type Continuation = Vec<Operation>;
type Code = Vec<u8>;

fn read_code(path: &Path) -> io::Result<Code> {
    std::fs::read(path)
}

#[derive(Debug, Clone)]
enum Operation {
    Nop,
    Push(Symbol),
    Digit(u8),
    Plus,
    Minus,
    Log,
    Output,
    Input,
    Enqueue,
    Dequeue,
    Dup,
    Supplant,
    Fetch,
    Sequence(Rc<[Operation]>),
}

struct State {
    interpreter: [Operation; 256],
    stack: Vec<Symbol>,
    queue: VecDeque<Symbol>,
    continuation: Continuation,
}

enum EmmentalError {
    Underflow,
    QueueEmpty,
    Io(io::Error),
}

impl From<io::Error> for EmmentalError {
    fn from(err: io::Error) -> Self {
        Self::Io(err)
    }
}

type Result<T> = core::result::Result<T, EmmentalError>;

impl State {
    fn init_interpreter() -> [Operation; 256] {
        use Operation as O;
        let mut interpreter = {
            const NOP: Operation = O::Nop;
            [NOP; 256]
        };
        interpreter[b'#' as usize] = O::Push(0);
        for digit in b'0'..=b'9' {
            interpreter[digit as usize] = O::Digit(digit - b'0');
        }
        interpreter[b'+' as usize] = O::Plus;
        interpreter[b'-' as usize] = O::Minus;
        interpreter[b'~' as usize] = O::Log;
        interpreter[b'.' as usize] = O::Output;
        interpreter[b',' as usize] = O::Input;
        interpreter[b'^' as usize] = O::Enqueue;
        interpreter[b'v' as usize] = O::Dequeue;
        interpreter[b':' as usize] = O::Dup;
        interpreter[b'!' as usize] = O::Supplant;
        interpreter[b'?' as usize] = O::Fetch;
        interpreter[b';' as usize] = O::Push(b';');
        interpreter
    }

    fn new(code: Code) -> Self {
        Self {
            interpreter: Self::init_interpreter(),
            stack: vec![],
            queue: VecDeque::new(),
            continuation: Self::top_level_continuation(code)
        }
    }

    fn top_level_continuation(code: Code) -> Continuation {
        let mut operations = Vec::new();
        for b in code {
            use Operation as O;
            operations.push(O::Push(b));
            operations.push(O::Fetch);
        }
        operations.reverse();
        operations
    }

    fn perform_op(&mut self, operation: Operation) -> Result<()> {
        use Operation as O;
        match operation {
            O::Nop => {}
            O::Push(b) => self.stack.push(b),
            O::Digit(d) => {
                let a = self.pop()?;
                self.stack.push(a.wrapping_mul(10).wrapping_add(d));
            }
            O::Plus => {
                let b = self.pop()?;
                let a = self.pop()?;
                self.stack.push(u8::wrapping_add(a, b));
            }
            O::Minus => {
                let b = self.pop()?;
                let a = self.pop()?;
                self.stack.push(u8::wrapping_sub(a, b));
            }
            O::Log => {
                let a = self.pop()?;
                self.stack.push(Self::ilog2(a));
            }
            O::Output => {
                use io::Write as _;
                let a = self.pop()?;
                io::stdout().write_all(&[a])?;
            }
            O::Input => {
                use io::Read as _;
                let byte = io::stdin().bytes().next().transpose()?;
                if let Some(b) = byte {
                    self.stack.push(b);
                }
            }
            O::Enqueue => {
                let a = self.peek()?;
                self.queue.push_back(a)
            }
            O::Dequeue => {
                let a = self.queue.pop_front().ok_or(EmmentalError::QueueEmpty)?;
                self.stack.push(a);
            }
            O::Dup => {
                let a = self.peek()?;
                self.stack.push(a)
            }
            O::Supplant => {
                self.supplant()?;
            }
            O::Fetch => {
                let b = self.pop()?;
                self.continuation.push(self.interpreter[b as usize].clone());
            }
            O::Sequence(seq) => {
                self.continuation.extend(seq.iter().cloned())
            }
        }
        Ok(())
    }

    fn supplant(&mut self) -> Result<()> {
        use Operation as O;
        let a = self.pop()?;
        // TODO: simplify things like `#123` -> `O::Push(123)`
        let mut new_sequence = Vec::new();
        loop {
            let sym = self.pop()?;
            if sym == b';' {
                break;
            }
            new_sequence.push(self.interpreter[sym as usize].clone());
        }
        let operation = match new_sequence.len() {
            0 => O::Nop,
            1 => new_sequence.pop().expect("len == 1"),
            _ => O::Sequence(new_sequence.into_boxed_slice().into()),
        };
        self.interpreter[a as usize] = operation;
        Ok(())
    }

    fn pop_stack(stack: &mut Vec<Symbol>) -> Result<Symbol> {
        stack.pop().ok_or(EmmentalError::Underflow)
    }

    fn pop(&mut self) -> Result<Symbol> {
        Self::pop_stack(&mut self.stack)
    }

    fn peek(&mut self) -> Result<Symbol> {
        self.stack.last().copied().ok_or(EmmentalError::Underflow)
    }

    fn ilog2(x: u8) -> u8 {
        x.checked_ilog2().unwrap_or(8) as u8
    }

    fn run(&mut self) -> Result<()> {
        while let Some(op) = {
            // eprintln!("cont = {:?}", self.continuation);
            self.continuation.pop()
        } {
            self.perform_op(op)?;
        }
        Ok(())
    }
}

fn main() -> io::Result<()> {
    let Some(path) = std::env::args_os().nth(1) else {
        eprintln!("usage: emmental [file]");
        return Ok(());
    };
    let code = read_code(path.as_ref())?;
    let mut state = State::new(code);
    if let Err(e) = state.run() {
        match e {
            EmmentalError::Underflow => {
                eprintln!("error: stack underflow");
                std::process::exit(2)
            }
            EmmentalError::QueueEmpty => {
                eprintln!("error: queue underflow");
                std::process::exit(2)
            }
            EmmentalError::Io(e) => {
                return Err(e)
            }
        }
    }
    Ok(())
}
