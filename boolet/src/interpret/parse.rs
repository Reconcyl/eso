use arena::Arena;

use super::Subprogram;

/// Unlike Underload, Boolet has no way to construct functions at runtime. This means that the
/// parser can return an arena of all functions used in the program, and function objects on the
/// stack can simply store indices into the function pool rather than needing to own subprograms.
#[derive(Debug, Clone)]
pub(super) struct FunctionArena(Arena<Function>);

pub(super) struct Function {
    code: Subprogram,
    start: Location,
    end: Location,
}

/// Represents a location in the program.
#[derive(Debug, Clone, Copy)]
pub(super) struct Location(usize)