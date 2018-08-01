/// Represents the type of a command.
///
/// It is parameterized over the lifetime of the function arena that
/// function invocations refer to.
#[derive(Debug)]
pub enum CommandType<'a> {
    One,
    Zero,
    Function(&'a Subprogram<'a>),
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

/// Represents the location of a command within a program.
#[derive(Debug, Clone, Copy)]
pub struct Location(usize);

impl Location {
    /// Return the row and column that this location refers to in a list of lines.
    fn locate_within<I: Iterator<Item=&str>>(self, lines: I) -> Option<(usize, usize)> {
        let mut characters_left = self.0;
        for (line_no, line) in lines.enumerate() {
            for (col_no, _) in line.chars().enumerate() {
                if chars_left == 0 {
                    return (line_no, col_no);
                } else {
                    chars_left -= 1;
                }
            }
        }
    }
}

/// Represents the location and type of a command.
///
/// It is parameterized over the lifetime of the function arena that
/// function invocations refer to.
#[derive(Debug)]
pub struct Command<'a> {
    pub location: Location,
    pub type_: CommandType<'a>,
}

/// A subprogram is a list of commands.
///
/// It is parameterized over the lifetime of the function arena that
/// function invocations refer to.
pub type Subprogram<'a> = Vec<Command<'a>>;

/// Unlike Underload, Boolet has no way to construct functions at
/// runtime. This means that the parser can return a pool of all
/// functions used in the program, and function objects on the stack
/// can simply store references into the function pool, and we don't
/// need to copy subprograms around.
#[derive(Debug)]
pub(super) struct FunctionArena(Arena<Subprogram<'a>>);