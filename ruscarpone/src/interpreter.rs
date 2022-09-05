use std::collections::HashMap;
use std::io::{self, Read, Write};
use std::mem;
use std::ops::Deref;
use std::rc::Rc;

pub struct State<R, W> {
    current_interpreter: Rc<Interpreter>,
    stack: Vec<Object>,
    input: io::Bytes<R>,
    output: W,
    nesting: u32,
}

type Symbol = char;

#[derive(Clone)]
enum Object {
    Symbol(Symbol),
    Operation(Operation),
    Interpreter(Rc<Interpreter>),
}

#[derive(Clone)]
struct Interpreter {
    parent: Option<Rc<Self>>,
    dict: HashMap<Symbol, Operation>,
    // In the case that the character dictionary of an interpreter does not contain a definition
    // for a symbol, we also try a fallback function. This is so that interpreters that have a
    // definition for every character (such as the interpreter pushed by `1` or enabled by `'`
    // don't have to fill in every character.
    fallback: Rc<dyn Fn(Symbol) -> Option<Operation> + 'static>,
}

#[derive(Clone)]
enum Operation {
    Builtin(BuiltinOperation),
    Quotesym(Box<(Symbol, Rc<Interpreter>)>),
    Deepquote(Box<(Symbol, Rc<Interpreter>)>),
    Custom(Rc<CustomOperation>),
}

#[derive(Clone, Copy)]
enum BuiltinOperation {
    Nop,
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
    Dup,
    Pop,
    Swap,
}

struct CustomOperation {
    context: Rc<Interpreter>,
    definition: String,
}

impl<R: Read, W: Write> State<R, W> {
    pub fn new(input: R, output: W) -> Self {
        State {
            current_interpreter: Rc::new(Interpreter::initial()),
            stack: Vec::new(),
            input: input.bytes(),
            output,
            nesting: 0,
        }
    }
    fn push_any(&mut self, obj: Object) {
        self.stack.push(obj);
    }
    fn push(&mut self, elem: impl ObjectType) {
        self.push_any(elem.upcast());
    }
    fn pop_any(&mut self) -> Result<Object, String> {
        self.stack
            .pop()
            .ok_or_else(|| "Tried to pop from empty stack".to_owned())
    }
    fn pop<T: ObjectType>(&mut self) -> Result<T, String> {
        let element = self
            .stack
            .pop()
            .ok_or_else(|| format!("Tried to pop {} from empty stack", T::name()))?;
        T::downcast(&element).ok_or_else(|| format!(
            "Tried to pop {}, got {}",
            T::name(),
            element.name()
        ))
    }
    fn pop_string(&mut self) -> Result<String, String> {
        match self.stack.last() {
            None => return Err("Tried to pop string from empty stack".to_owned()),
            Some(Object::Symbol(c)) => {
                if *c != ']' {
                    return Err(format!(
                        "Expected symbol ']' at end of string, got symbol '{}'",
                        c
                    ));
                }
            }
            Some(o) => return Err(format!("Expected string, got {}", o.name())),
        }
        // scan the stack backwards to find the matching `[`
        let string_end = self.stack.len() - 1;
        let mut idx = string_end;
        let mut nesting = 1u32;
        loop {
            if idx == 0 {
                return Err("Tried to pop string, got end of stack".into());
            }
            idx -= 1;
            match &self.stack[idx] {
                Object::Symbol(']') => nesting += 1,
                Object::Symbol('[') => nesting -= 1,
                Object::Symbol(_) => {}
                o => {
                    return Err(format!(
                        "Expected symbol while popping string, got {}",
                        o.name()
                    ))
                }
            }
            if nesting == 0 {
                break;
            }
        }
        let string_start = idx + 1;
        // read the string contents starting from this index
        let str = self.stack[string_start..string_end]
            .iter()
            .map(|o| match o {
                Object::Symbol(c) => c,
                _ => unreachable!(),
            })
            .collect();
        self.stack.truncate(idx);
        Ok(str)
    }
    fn push_string(&mut self, string: &str) {
        self.push('[');
        for symbol in string.chars() {
            self.push(symbol);
        }
        self.push(']');
    }
    fn run_operation(&mut self, o: &Operation) -> Result<(), String> {
        match o {
            Operation::Builtin(o) => match o {
                BuiltinOperation::Nop => {}
                BuiltinOperation::Reify => self.push(Rc::clone(&self.current_interpreter)),
                BuiltinOperation::Deify => self.current_interpreter = Rc::clone(&self.pop()?),
                BuiltinOperation::Extract => {
                    let symbol = self.pop()?;
                    let interpreter: Rc<Interpreter> = self.pop()?;
                    self.push(interpreter.get_action(symbol)?);
                }
                BuiltinOperation::Install => {
                    let symbol = self.pop()?;
                    let oper = self.pop()?;
                    let interpreter = self.pop()?;
                    self.push(Interpreter::set_action(interpreter, symbol, oper));
                }
                BuiltinOperation::GetParent => {
                    let interpreter: Rc<Interpreter> = self.pop()?;
                    self.push(interpreter.get_parent());
                }
                BuiltinOperation::SetParent => {
                    let i = self.pop()?;
                    let j = self.pop()?;
                    self.push(Interpreter::set_parent(i, j));
                }
                BuiltinOperation::Create => {
                    let context = self.pop()?;
                    let definition = self.pop_string()?;
                    self.push(Operation::Custom(Rc::new(CustomOperation {
                        context,
                        definition,
                    })))
                }
                // This is an unhelpful instruction, so I'm implementing it in the most unhelpful way
                // possible.
                BuiltinOperation::Expand => {
                    let oper = self.pop()?;
                    self.push_string("@");
                    self.push(Rc::new(Interpreter::uniform(oper)));
                }
                BuiltinOperation::Perform => {
                    let oper = self.pop()?;
                    self.run_operation(&oper)?;
                }
                BuiltinOperation::Null => self.push(Rc::new(Interpreter::null())),
                BuiltinOperation::Uniform => {
                    let oper = self.pop()?;
                    self.push(Rc::new(Interpreter::uniform(oper)));
                }
                BuiltinOperation::Deepquote => {
                    self.nesting = 1;
                    self.push('[');
                    self.current_interpreter = Rc::new(Interpreter::deep_quote(Rc::clone(
                        &self.current_interpreter,
                    )));
                }
                BuiltinOperation::Quotesym => {
                    self.current_interpreter =
                        Rc::new(Interpreter::quote(Rc::clone(&self.current_interpreter)));
                }
                BuiltinOperation::Output => {
                    let symbol = self.pop()?;
                    self.write_symbol(symbol)?;
                }
                BuiltinOperation::Input => {
                    let symbol = self.read_symbol()?;
                    self.push(symbol);
                }
                BuiltinOperation::Dup => {
                    let e = self.pop_any()?;
                    self.push_any(e.clone());
                    self.push_any(e);
                }
                BuiltinOperation::Pop => {
                    drop(self.pop_any()?);
                }
                BuiltinOperation::Swap => {
                    let a = self.pop_any()?;
                    let b = self.pop_any()?;
                    self.push_any(a);
                    self.push_any(b);
                }
            },
            Operation::Quotesym(b) => {
                let (s, old_interpreter) = b.deref().clone();
                self.push(s);
                self.current_interpreter = old_interpreter;
            }
            Operation::Deepquote(b) => {
                let &(s, ref old_interpreter) = b.deref();
                self.push(s);
                if s == ']' {
                    self.nesting -= 1;
                    if self.nesting == 0 {
                        self.current_interpreter = Rc::clone(old_interpreter);
                    }
                } else if s == '[' {
                    self.nesting += 1;
                }
            }
            Operation::Custom(custom) => {
                let old_interpreter =
                    mem::replace(&mut self.current_interpreter, Rc::clone(&custom.context));
                self.run(custom.definition.chars())?;
                self.current_interpreter = old_interpreter;
            }
        }
        Ok(())
    }
    pub fn run_symbol(&mut self, s: Symbol) -> Result<(), String> {
        let oper = self.current_interpreter.get_action(s)?;
        self.run_operation(&oper)
    }
    pub fn run<I: IntoIterator<Item = Symbol>>(&mut self, symbols: I) -> Result<(), String> {
        for s in symbols.into_iter() {
            self.run_symbol(s)?;
        }
        Ok(())
    }
    fn write_symbol(&mut self, symbol: Symbol) -> Result<(), String> {
        match self.output.write(&symbol.to_string().as_bytes()) {
            Ok(_size) => Ok(()),
            Err(e) => Err(e.to_string()),
        }
    }
    fn read_symbol(&mut self) -> Result<Symbol, String> {
        match self.input.next() {
            None => return Err(String::from("End of input")),
            Some(Err(e)) => return Err(e.to_string()),
            Some(Ok(byte)) => Ok(byte as char),
        }
    }
}

fn initial_dict() -> HashMap<Symbol, Operation> {
    [
        ('v', BuiltinOperation::Reify),
        ('^', BuiltinOperation::Deify),
        ('>', BuiltinOperation::Extract),
        ('<', BuiltinOperation::Install),
        ('{', BuiltinOperation::GetParent),
        ('}', BuiltinOperation::SetParent),
        ('*', BuiltinOperation::Create),
        ('@', BuiltinOperation::Expand),
        ('!', BuiltinOperation::Perform),
        ('0', BuiltinOperation::Null),
        ('1', BuiltinOperation::Uniform),
        ('[', BuiltinOperation::Deepquote),
        ('\'', BuiltinOperation::Quotesym),
        ('.', BuiltinOperation::Output),
        (',', BuiltinOperation::Input),
        (':', BuiltinOperation::Dup),
        ('$', BuiltinOperation::Pop),
        ('/', BuiltinOperation::Swap),
    ]
    .map(|(c, op)| (c, Operation::Builtin(op)))
    .into()
}

impl Interpreter {
    fn null() -> Self {
        Self {
            parent: None,
            dict: HashMap::new(),
            fallback: Rc::new(|_| None),
        }
    }
    fn uniform(oper: Operation) -> Self {
        Self {
            parent: None,
            dict: HashMap::new(),
            fallback: Rc::new(move |_| Some(oper.clone())),
        }
    }
    fn initial() -> Self {
        Self {
            parent: None,
            dict: initial_dict(),
            fallback: Rc::new(|_| Some(Operation::Builtin(BuiltinOperation::Nop))),
        }
    }
    fn quote(original: Rc<Self>) -> Self {
        Self {
            parent: None,
            dict: HashMap::new(),
            fallback: Rc::new(move |s| {
                let original = Rc::clone(&original);
                Some(Operation::Quotesym(Box::new((s, original))))
            }),
        }
    }
    fn deep_quote(original: Rc<Self>) -> Self {
        Self {
            parent: None,
            dict: HashMap::new(),
            fallback: Rc::new(move |s| {
                let original = Rc::clone(&original);
                Some(Operation::Deepquote(Box::new((s, original))))
            }),
        }
    }
    fn get_parent(&self) -> Rc<Self> {
        self.parent
            .as_ref()
            .map(Rc::clone)
            .unwrap_or_else(|| Rc::new(Interpreter::null()))
    }
    fn set_parent(original: Rc<Self>, parent: Rc<Self>) -> Rc<Self> {
        // Modify the interpreter in-place if possible; otherwise, clone its
        // character dictionary and fallback functions and make a new one.
        Rc::new(match Rc::try_unwrap(original) {
            Ok(mut i) => {
                i.parent = Some(parent);
                i
            }
            Err(original) => Interpreter {
                parent: Some(parent),
                dict: original.dict.clone(),
                fallback: Rc::clone(&original.fallback),
            },
        })
    }
    fn get_action(&self, s: Symbol) -> Result<Operation, String> {
        self.dict
            .get(&s)
            .cloned()
            .or_else(|| (self.fallback)(s))
            .ok_or_else(|| format!("Interpreter has no definition for {}", s))
    }
    fn set_action(original: Rc<Self>, s: Symbol, oper: Operation) -> Rc<Self> {
        let mut new_interpreter = match Rc::try_unwrap(original) {
            Ok(i) => i,
            Err(original) => (*original).clone(),
        };
        new_interpreter.dict.insert(s, oper);
        Rc::new(new_interpreter)
    }
}

trait ObjectType: Sized {
    fn name() -> &'static str;
    fn downcast(_: &Object) -> Option<Self>;
    fn upcast(self) -> Object;
}

macro_rules! object_type {
    ($type:ty, $tag:path, $name:expr) => {
        impl ObjectType for $type {
            fn name() -> &'static str {
                $name
            }
            fn downcast(obj: &Object) -> Option<Self> {
                match *obj {
                    $tag(ref a) => Some(a.clone()),
                    _ => None,
                }
            }
            fn upcast(self) -> Object {
                $tag(self)
            }
        }
    };
}

object_type!(Symbol, Object::Symbol, "symbol");
object_type!(Operation, Object::Operation, "operation");
object_type!(Rc<Interpreter>, Object::Interpreter, "interpreter");

impl Object {
    fn name(&self) -> &'static str {
        match *self {
            Object::Symbol(_) => Symbol::name(),
            Object::Operation(_) => Operation::name(),
            Object::Interpreter(_) => <Rc<Interpreter>>::name(),
        }
    }
}
