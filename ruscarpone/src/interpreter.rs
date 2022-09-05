use ::std::collections::HashMap;
use ::std::rc::Rc;
use ::std::io::{self, Read, Write};
use ::std::mem;

pub fn fmt_to_string<T: ::std::fmt::Display>(t: T) -> String {
    format!("{}", t)
}

type Symbol = char;

enum Object<'a, R, W> {
    Symbol(Symbol),
    Action(Rc<Action<'a, R, W>>),
    Operation(Operation<Rc<Interpreter<'a, R, W>>>),
    Interpreter(Rc<Interpreter<'a, R, W>>),
}

// We can't just use `#[derive(Clone)]` to generate a Clone implementation for us. This is a known
// limitation and is apparently unsolvable.
impl<'a, R: 'a, W: 'a> Clone for Object<'a, R, W> {
    fn clone(&self) -> Self {
        match self {
            Object::Symbol(s) => Object::Symbol(*s),
            Object::Action(o) => Object::Action(Rc::clone(o)),
            Object::Operation(o) => Object::Operation(o.clone()),
            Object::Interpreter(i) => Object::Interpreter(Rc::clone(i))
        }
    }
}

struct Interpreter<'a, R, W> {
    parent: Option<Rc<Interpreter<'a, R, W>>>,
    dict: HashMap<Symbol, Rc<Action<'a, R, W>>>,
    // In the case that the character dictionary of an interpreter does not contain a definition
    // for a symbol, we also try a fallback function. This is so that interpreters that have a
    // definition for every character (such as the interpreter pushed by `1` or enabled by `'`
    // don't have to fill in every character.
    fallback: Rc<dyn Fn(Symbol) -> Option<Rc<Action<'a, R, W>>> + 'a>,
}

impl<'a, R: 'a, W: 'a> Clone for Interpreter<'a, R, W> {
    fn clone(&self) -> Self {
        Interpreter {
            parent: self.parent.clone(),
            dict: self.dict.clone(),
            fallback: Rc::clone(&self.fallback)
        }
    }
}

pub struct State<'a, R, W> {
    current_interpreter: Rc<Interpreter<'a, R, W>>,
    stack: Vec<Object<'a, R, W>>,
    input: io::Bytes<R>,
    output: W,
    nesting: u32,
}

#[derive(Clone)]
enum Operation<InterpreterPtr> {
    Builtin(BuiltinOperation),
    Custom(Rc<CustomOperation<InterpreterPtr>>),
}

#[derive(Clone, Copy)]
enum BuiltinOperation {
    Reify,
    Deify,
    Extract,
    Install,
    GetParent,
    SetParent,
    Create,
    Expand,
    Perform,
    Null,
    Uniform,
    Deepquote,
    Quotesym,
    Output,
    Input,
}

struct CustomOperation<InterpreterPtr> {
    context: InterpreterPtr,
    definition: String,
}

type Action<'a, R, W> = dyn Fn(&mut State<'a, R, W>) -> Result<(), String> + 'a;

// Constucton an `Rc<Action>` out of a closure.
fn action<'a, R, W, F>(closure: F) -> Rc<Action<'a, R, W>>
where
    F: Fn(&mut State<'a, R, W>) -> Result<(), String> + 'a,
{
    Rc::new(closure)
}

fn operation<'a, R: Read + 'a, W: Write + 'a>(
    o: Operation<Rc<Interpreter<'a, R, W>>>
) -> Rc<Action<'a, R, W>> {
    action(move |state| state.run_operation(&o))
}

trait ObjectType<'a, R, W>: Sized {
    fn name() -> &'static str;
    fn downcast(_: &Object<'a, R, W>) -> Option<Self>;
}

macro_rules! gen_object_downcast_impl {
    ($type:ty, $tag:path, $name:expr) => {
        impl<'a, R, W> ObjectType<'a, R, W> for $type {
            fn name() -> &'static str { $name }
            fn downcast(obj: &Object<'a, R, W>) -> Option<Self> {
                match *obj {
                    $tag(ref a) => Some(a.clone()),
                    _ => None,
                }
            }
        }
    }
}

gen_object_downcast_impl!(Symbol, Object::Symbol, "symbol");
gen_object_downcast_impl!(Rc<Action<'a, R, W>>, Object::Action, "action");
gen_object_downcast_impl!(Operation<Rc<Interpreter<'a, R, W>>>, Object::Operation, "operation");
gen_object_downcast_impl!(Rc<Interpreter<'a, R, W>>, Object::Interpreter, "interpreter");

impl<'a, R: Read + 'a, W: Write + 'a> Object<'a, R, W> {
    fn name(&self) -> &'static str {
        match *self {
            Object::Symbol(_) => <Symbol as ObjectType<R, W>>::name(),
            Object::Action(_) => <Rc<Action<R, W>>>::name(),
            Object::Operation(_) => <Operation<Rc<Interpreter<'a, R, W>>>>::name(),
            Object::Interpreter(_) => <Rc<Interpreter<R, W>>>::name(),
        }
    }
}

impl<'a, R: Read + 'a, W: Write + 'a> Interpreter<'a, R, W> {
    fn null() -> Self {
        Self {
            parent: None,
            dict: HashMap::new(),
            fallback: Rc::new(|_| None),
        }
    }
    fn uniform(action: Rc<Action<'a, R, W>>) -> Self {
        Self {
            parent: None,
            dict: HashMap::new(),
            fallback: Rc::new(move |_| Some(Rc::clone(&action)))
        }
    }
    fn initial() -> Self {
        Self {
            parent: None,
            dict: initial_dict::initial_dict(),
            fallback: Rc::new(|_| Some(action(|_| Ok(()))))
        }
    }
    fn quote(original: Rc<Interpreter<'a, R, W>>) -> Self {
        Self {
            parent: None,
            dict: HashMap::new(),
            fallback: Rc::new(move |s| {
                let original = Rc::clone(&original);
                Some(action(move |state| {
                    state.stack.push(Object::Symbol(s));
                    state.current_interpreter = Rc::clone(&original);
                    Ok(())
                }))
            })
        }
    }
    fn deep_quote(original: Rc<Interpreter<'a, R, W>>) -> Self {
        Self {
            parent: None,
            dict: HashMap::new(),
            fallback: Rc::new(move |s| {
                let original = Rc::clone(&original);
                Some(action(move |state| {
                    state.stack.push(Object::Symbol(s));
                    if s == ']' {
                        state.nesting -= 1;
                        if state.nesting == 0 {
                            state.current_interpreter = Rc::clone(&original);
                        }
                    } else if s == '[' {
                        state.nesting += 1;
                    }
                    Ok(())
                }))
            })
        }
    }
    fn get_parent(&self) -> Rc<Interpreter<'a, R, W>> {
        self.parent.as_ref()
            .map(Rc::clone)
            .unwrap_or_else(|| Rc::new(Interpreter::null()))
    }
    fn set_parent(original: Rc<Interpreter<'a, R, W>>,
                  parent: Rc<Interpreter<'a, R, W>>) -> Rc<Interpreter<'a, R, W>> {
        // Modify the interpreter in-place if possible; otherwise, clone its
        // character dictionary and fallback functions and make a new one.
        Rc::new(match Rc::try_unwrap(original) {
            Ok(mut i) => {
                i.parent = Some(parent);
                i
            }
            Err(original) => {
                Interpreter {
                    parent: Some(parent),
                    dict: original.dict.clone(),
                    fallback: Rc::clone(&original.fallback)
                }
            }
        })
    }
    fn get_action(&self, s: Symbol) -> Result<Rc<Action<'a, R, W>>, String> {
        self.dict.get(&s).map(Rc::clone)
            .or_else(|| (self.fallback)(s).map(Into::into))
            .ok_or_else(|| format!("Interpreter has no definition for {}", s))
    }
    fn set_action(original: Rc<Interpreter<'a, R, W>>,
                     s: Symbol, a: Rc<Action<'a, R, W>>) -> Rc<Interpreter<'a, R, W>> {
        let mut new_interpreter = match Rc::try_unwrap(original) {
            Ok(i) => i,
            Err(original) => (*original).clone(),
        };
        new_interpreter.dict.insert(s, a);
        Rc::new(new_interpreter)
    }
}

impl<'a, R: Read + 'a, W: Write + 'a> State<'a, R, W> {
    pub fn new(input: R, output: W) -> Self {
        State {
            current_interpreter: Rc::new(Interpreter::initial()),
            stack: Vec::new(),
            input: input.bytes(),
            output,
            nesting: 0,
        }
    }
    fn pop(&mut self) -> Option<Object<'a, R, W>> {
        self.stack.pop()
    }
    fn pop_any(&mut self) -> Result<Object<'a, R, W>, String> {
        self.pop().ok_or("Tried to pop from empty stack".to_string())
    }
    fn pop_as<T: ObjectType<'a, R, W>>(&mut self) -> Result<T, String> {
        let element = self.pop()
            .ok_or(format!("Tried to pop {} from empty stack", T::name()))?;
        T::downcast(&element)
            .ok_or(format!("Tried to pop {}, got {}", T::name(), element.name()))
    }
    fn pop_string(&mut self) -> Result<Vec<Symbol>, String> {
        // Expect a ']'.
        match self.pop_as()? {
            ']' => {}
            o => return Err(format!("Tried to pop string, got symbol {:?}", o))
        }
        // Read the characters of the string from the stack in reverse order.
        let mut characters = Vec::new();
        let mut nesting = 1;
        while nesting > 0 {
            let c: Symbol = self.pop_as()?;
            if c == ']' {
                nesting += 1;
            }
            else if c == '[' {
                nesting -= 1;
            }
            characters.push(c);
        }
        // This includes the final opening '[', so we remove that.
        characters.pop();
        characters.reverse();
        Ok(characters)
    }
    fn push_string(&mut self, string: &str) {
        self.stack.push(Object::Symbol('['));
        for symbol in string.chars() {
            self.stack.push(Object::Symbol(symbol));
        }
        self.stack.push(Object::Symbol(']'));
    }
    fn run_operation(&mut self, o: &Operation<Rc<Interpreter<'a, R, W>>>) -> Result<(), String> {
        match o {
            Operation::Builtin(o) => match o {
                BuiltinOperation::Reify => {
                    let current = Rc::clone(&self.current_interpreter);
                    self.stack.push(Object::Interpreter(current));
                }
                BuiltinOperation::Deify => {
                    self.current_interpreter = Rc::clone(&self.pop_as()?);
                }
                BuiltinOperation::Extract => {
                    let symbol = self.pop_as()?;
                    let interpreter: Rc<Interpreter<_, _>> = self.pop_as()?;
                    let action = interpreter.get_action(symbol)?;
                    self.stack.push(Object::Action(action));
                }
                BuiltinOperation::Install => {
                    let symbol = self.pop_as()?;
                    let operation = self.pop_as()?;
                    let interpreter = self.pop_as()?;
                    let new_interpreter = Interpreter::set_action(
                        interpreter,
                        symbol,
                        operation
                    );
                    self.stack.push(Object::Interpreter(new_interpreter));
                }
                BuiltinOperation::GetParent => {
                    let old_interpreter: Rc<Interpreter<_, _>> = self.pop_as()?;
                    let parent = old_interpreter.get_parent();
                    self.stack.push(Object::Interpreter(parent));
                }
                BuiltinOperation::SetParent => {
                    let i = self.pop_as()?;
                    let j = self.pop_as()?;
                    let new = Interpreter::set_parent(i, j);
                    self.stack.push(Object::Interpreter(new));
                }
                BuiltinOperation::Create => {
                    let interpreter: Rc<Interpreter<R, W>> = self.pop_as()?;
                    let string = self.pop_string()?;
                    self.stack.push(Object::Action(action(move |state| {
                        let old_interpreter = mem::replace(
                            &mut state.current_interpreter,
                            Rc::clone(&interpreter));
                        state.run(string.clone())?;
                        state.current_interpreter = old_interpreter;
                        Ok(())
                    })));
                }
                // This is an unhelpful instruction, so I'm implementing it in the most unhelpful way
                // possible.
                BuiltinOperation::Expand => {
                    let action = self.pop_as()?;
                    self.push_string("@");
                    let interpreter = Interpreter::uniform(action);
                    self.stack.push(Object::Interpreter(Rc::new(interpreter)));
                }
                BuiltinOperation::Perform => {
                    let action: Rc<Action<_, _>> = self.pop_as()?;
                    action(self)?;
                }
                BuiltinOperation::Null => {
                    self.stack.push(Object::Interpreter(Rc::new(Interpreter::null())));
                }
                BuiltinOperation::Uniform => {
                    let action = self.pop_as()?;
                    let interpreter = Interpreter::uniform(action);
                    self.stack.push(Object::Interpreter(Rc::new(interpreter)));
                }
                BuiltinOperation::Deepquote => {
                    self.nesting = 1;
                    self.stack.push(Object::Symbol('['));
                    self.current_interpreter = Rc::new(
                        Interpreter::deep_quote(Rc::clone(
                            &self.current_interpreter)));
                }
                BuiltinOperation::Quotesym => {
                    self.current_interpreter = Rc::new(
                        Interpreter::quote(Rc::clone(
                            &self.current_interpreter)));
                }
                BuiltinOperation::Output => {
                    let symbol = self.pop_as()?;
                    self.write_symbol(symbol)?;
                }
                BuiltinOperation::Input =>
                    match self.input.next() {
                        None => return Err(String::from("End of input")),
                        Some(Err(e)) => return Err(format!("{}", e)),
                        Some(Ok(byte)) => {
                            self.stack.push(Object::Symbol(byte as char));
                        }
                    }
            },
            Operation::Custom(_) => todo!(),
        }
        Ok(())
    }
    pub fn run_symbol(&mut self, s: Symbol) -> Result<(), String> {
        self.current_interpreter.get_action(s)?(self)
    }
    pub fn run<I: IntoIterator<Item=Symbol>>(&mut self, symbols: I) -> Result<(), String> {
        for s in symbols.into_iter() {
            self.run_symbol(s)?;
        }
        Ok(())
    }
    fn write_symbol(&mut self, symbol: Symbol) -> Result<(), String> {
        match self.output.write(&symbol.to_string().as_bytes()) {
            Ok(_size) => Ok(()),
            Err(e) => Err(fmt_to_string(e))
        }
    }
}

mod initial_dict {
    use super::*;
    pub(super) fn initial_dict<'a, R: Read + 'a, W: Write + 'a>()
        -> HashMap<Symbol, Rc<Action<'a, R, W>>>
    {
        let mut map = HashMap::new();
        map.insert('v', operation(Operation::Builtin(BuiltinOperation::Reify)));
        map.insert('^', operation(Operation::Builtin(BuiltinOperation::Deify)));
        map.insert('>', operation(Operation::Builtin(BuiltinOperation::Extract)));
        map.insert('<', operation(Operation::Builtin(BuiltinOperation::Install)));
        map.insert('{', operation(Operation::Builtin(BuiltinOperation::GetParent)));
        map.insert('}', operation(Operation::Builtin(BuiltinOperation::SetParent)));
        map.insert('*', operation(Operation::Builtin(BuiltinOperation::Create)));
        map.insert('@', operation(Operation::Builtin(BuiltinOperation::Expand)));
        map.insert('!', operation(Operation::Builtin(BuiltinOperation::Perform)));
        map.insert('0', operation(Operation::Builtin(BuiltinOperation::Null)));
        map.insert('1', operation(Operation::Builtin(BuiltinOperation::Uniform)));
        map.insert('[', operation(Operation::Builtin(BuiltinOperation::Deepquote)));
        map.insert('\'', operation(Operation::Builtin(BuiltinOperation::Quotesym)));
        map.insert('.', operation(Operation::Builtin(BuiltinOperation::Output)));
        map.insert('.', operation(Operation::Builtin(BuiltinOperation::Input)));
        map.insert(':', action(|state| {
            let e = state.pop_any()?;
            state.stack.push(e.clone());
            state.stack.push(e);
            Ok(())
        }));
        map.insert('$', action(|state| {
            state.pop_any().map(|_| ())
        }));
        map.insert('/', action(|state| {
            let a = state.pop_any()?;
            let b = state.pop_any()?;
            state.stack.push(a);
            state.stack.push(b);
            Ok(())
        }));
        map
    }
}
