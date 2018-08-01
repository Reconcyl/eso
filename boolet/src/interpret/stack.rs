use super::parse::FunctionIndex;

#[derive(Debug, Clone, Copy)]
pub(super) enum Datum {
    Bit(bool),
    Function(FunctionIndex)
}

impl PartialEq for Datum {
    fn eq(&self, other: &Self) -> bool {
        // Don't even try to compare functions.
        match (self, other) {
            (Datum::Bit(a), Datum::Bit(b)) => a == b,
            _ => false
        }
    }
}

#[derive(Debug, Clone)]
pub(super) struct Stack {
    // All the stack items before the stack pointer.
    before: Vec<Datum>,
    // All the stack items after, stored in reverse.
    after: Vec<Datum>
}

impl Stack {
    pub(super) fn new() -> Self {
        Self {
            before: Vec::new(),
            after: Vec::new()
        }
    }
    pub(super) fn push(&mut self, datum: Datum) {
        self.before.push(datum);
    }
    pub(super) fn pop(&mut self) -> Option<Datum> {
        self.before.pop()
    }
    // Shift the stack pointer one element upwards; that is, closer to the TOS.
    pub(super) fn shift_up(&mut self) -> Option<()> {
        self.after.pop().map(|x| self.before.push(x))
    }
    pub(super) fn shift_down(&mut self) -> Option<()> {
        self.before.pop().map(|x| self.after.push(x))
    }
}
