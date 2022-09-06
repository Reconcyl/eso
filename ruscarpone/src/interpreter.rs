use std::collections::HashMap;
use std::io::{self, Read, Write};
use std::rc::Rc;

type Symbol = char;

pub struct State<R, W> {
    tasks: Vec<Task>,
    current_interpreter: Rc<Interpreter>,
    named_interpreters: InterpretersCtx,
    stack: Vec<Object>,
    input: io::Bytes<R>,
    output: W,
}

enum Task {
    Perform(Symbol),
    Backup,
}

struct InterpretersCtx {
    initial: Rc<Interpreter>,
    null: Rc<Interpreter>,
}

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
    archetype: InterpreterArchetype,
    fallback: InterpreterFallback,
}

// Metadata about the nature of the interpreter from which this one was derived.
#[derive(Clone)]
enum InterpreterArchetype {
    Initial,
    Other,
}

// In the case that the character dictionary of an interpreter does not contain a definition
// for a symbol, we also describe fallback behavior. This is so that interpreters that have a
// definition for every character (such as the interpreter pushed by `1` or enabled by `'`
// don't have to fill in every character.
#[derive(Clone)]
enum InterpreterFallback {
    Uniform(Operation),
    Quotesym,
    Deepquote,
}

#[derive(Clone)]
enum Operation {
    Builtin(BuiltinOperation),
    Quotesym(Symbol),
    Deepquote(Symbol),
    Custom(Rc<CustomOperation>),
}

impl PartialEq for Operation {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Operation::Builtin(b1), Operation::Builtin(b2)) => b1 == b2,
            (Operation::Quotesym(s1), Operation::Quotesym(s2)) => s1 == s2,
            (Operation::Deepquote(s1), Operation::Deepquote(s2)) => s1 == s2,
            (Operation::Custom(c1), Operation::Custom(c2)) => Rc::ptr_eq(c1, c2),
            _ => false,
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
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

    Illegal,
}

struct CustomOperation {
    context: Rc<Interpreter>,
    definition: String,
}

impl<R: Read, W: Write> State<R, W> {
    pub fn new(input: R, output: W) -> Self {
        let initial = Rc::new(Interpreter::initial());
        State {
            tasks: Vec::new(),
            current_interpreter: Rc::clone(&initial),
            named_interpreters: InterpretersCtx {
                initial,
                null: Rc::new(Interpreter::null()),
            },
            stack: Vec::new(),
            input: input.bytes(),
            output,
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
        T::downcast(&element)
            .ok_or_else(|| format!("Tried to pop {}, got {}", T::name(), element.name()))
    }
    fn pop_string(&mut self) -> Result<(String, usize), String> {
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
                return Err("Tried to pop string, got end of stack".to_owned());
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
        Ok((str, string_end - string_start))
    }
    fn save_interpreter(&mut self, new_interpreter: Rc<Interpreter>) {
        let current = Rc::clone(&self.current_interpreter);
        self.current_interpreter =
            Interpreter::set_parent(new_interpreter, current, &self.named_interpreters);
    }
    fn backup_interpreter(&mut self) {
        let parent = self
            .current_interpreter
            .get_parent(&self.named_interpreters);
        let parent = Rc::clone(&parent);
        self.current_interpreter = parent;
    }

    pub fn run<I: IntoIterator<Item = Symbol>>(&mut self, symbols: I) -> Result<(), String> {
        assert!(self.tasks.is_empty());
        for s in symbols.into_iter() {
            self.tasks.push(Task::Perform(s));
        }
        self.tasks.reverse(); // pop from back to front
        while let Some(task) = self.tasks.pop() {
            match task {
                Task::Perform(s) => {
                    let oper = self.current_interpreter.get_action(s);
                    self.run_operation(oper)?;
                }
                Task::Backup => self.backup_interpreter(),
            }
        }
        Ok(())
    }
    fn run_operation(&mut self, o: Operation) -> Result<(), String> {
        match o {
            Operation::Builtin(o) => match o {
                BuiltinOperation::Nop => {}
                BuiltinOperation::Reify => self.push(Rc::clone(&self.current_interpreter)),
                BuiltinOperation::Deify => self.current_interpreter = Rc::clone(&self.pop()?),
                BuiltinOperation::Extract => {
                    let symbol = self.pop()?;
                    let interpreter: Rc<Interpreter> = self.pop()?;
                    self.push(interpreter.get_action(symbol));
                }
                BuiltinOperation::Install => {
                    let symbol = self.pop()?;
                    let oper = self.pop()?;
                    let interpreter = self.pop()?;
                    self.push(Interpreter::set_action(interpreter, symbol, oper));
                }
                BuiltinOperation::GetParent => {
                    let interpreter: Rc<Interpreter> = self.pop()?;
                    let parent = interpreter.get_parent(&self.named_interpreters);
                    let parent = Rc::clone(parent);
                    self.push(parent);
                }
                BuiltinOperation::SetParent => {
                    let i = self.pop()?;
                    let j = self.pop()?;
                    self.push(Interpreter::set_parent(i, j, &self.named_interpreters));
                }
                BuiltinOperation::Create => {
                    let context: Rc<Interpreter> = self.pop()?;
                    let (definition, len) = self.pop_string()?;
                    self.push(match len {
                        0 => Operation::Builtin(BuiltinOperation::Nop),
                        1 => context.get_action(definition.chars().next().unwrap()),
                        _ => Operation::Custom(Rc::new(CustomOperation {
                            // we don't need to store this interpreter's parent, because:
                            // - when this operation is executed, this interpreter gets reified
                            //   with a different parent anyway
                            // - when this operation is inspected with `@`, this interpreter
                            //   gets pushed, but we don't make any guarantees about what
                            //   its parent should be
                            context: Interpreter::clear_parent(context),
                            definition,
                        })),
                    })
                }
                // This is an unhelpful instruction, so I'm implementing it in the most unhelpful way
                // possible.
                BuiltinOperation::Expand => {
                    let oper = self.pop()?;
                    self.expand_operation(oper);
                }
                BuiltinOperation::Perform => {
                    let oper = self.pop()?;
                    self.run_operation(oper)?;
                }
                BuiltinOperation::Null => self.push(Rc::clone(&self.named_interpreters.null)),
                BuiltinOperation::Uniform => {
                    let oper = self.pop()?;
                    self.push(Rc::new(Interpreter::uniform(oper)));
                }
                BuiltinOperation::Deepquote => {
                    self.push('[');
                    self.save_interpreter(Rc::new(Interpreter::deep_quote()));
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
                BuiltinOperation::Illegal => {
                    return Err("Illegal operation performed".to_owned());
                }
            },
            Operation::Quotesym(s) => {
                self.push(s);
                self.backup_interpreter();
            }
            Operation::Deepquote(s) => {
                self.push(s);
                if s == ']' {
                    self.backup_interpreter();
                } else if s == '[' {
                    self.save_interpreter(Rc::new(Interpreter::deep_quote()));
                }
            }
            Operation::Custom(custom) => {
                self.save_interpreter(Rc::clone(&custom.context));
                self.tasks.push(Task::Backup);
                let idx = self.tasks.len();
                self.tasks.extend(custom.definition.chars().map(Task::Perform));
                self.tasks[idx..].reverse(); // pop from back to front
            }
        }
        Ok(())
    }

    fn expand_operation(&mut self, o: Operation) {
        let interp;
        self.push('[');
        match o {
            Operation::Builtin(builtin) => match builtin {
                BuiltinOperation::Nop => interp = Rc::clone(&self.named_interpreters.null),
                BuiltinOperation::Reify
                | BuiltinOperation::Deify
                | BuiltinOperation::Extract
                | BuiltinOperation::Install
                | BuiltinOperation::GetParent
                | BuiltinOperation::SetParent
                | BuiltinOperation::Create
                | BuiltinOperation::Expand
                | BuiltinOperation::Perform
                | BuiltinOperation::Null
                | BuiltinOperation::Uniform
                | BuiltinOperation::Deepquote
                | BuiltinOperation::Quotesym
                | BuiltinOperation::Output
                | BuiltinOperation::Input
                | BuiltinOperation::Dup
                | BuiltinOperation::Pop
                | BuiltinOperation::Swap => {
                    self.push('_');
                    interp = Rc::new(Interpreter::uniform(o));
                }
                BuiltinOperation::Illegal => {
                    self.push('_');
                    interp = Rc::clone(&self.named_interpreters.null);
                }
            },
            Operation::Quotesym('[') => {
                for c in ['[', ']', '$', 'v', '{', '^'] {
                    self.push(c);
                }
                interp = Rc::clone(&self.named_interpreters.initial);
            }
            Operation::Quotesym(']') => {
                for c in ['[', ']', '/', '$', 'v', '{', '^'] {
                    self.push(c);
                }
                interp = Rc::clone(&self.named_interpreters.initial);
            }
            Operation::Quotesym(s) => {
                for c in ['\'', s, 'v', '{', '^'] {
                    self.push(c);
                }
                interp = Rc::clone(&self.named_interpreters.initial);
            }
            Operation::Deepquote('[') => {
                for c in ['v', '[', ']', '$', '>', '!'] {
                    self.push(c);
                }
                interp = Rc::clone(&self.named_interpreters.initial);
            }
            Operation::Deepquote(']') => {
                for c in ['[', ']', '/', '$', 'v', '{', '^'] {
                    self.push(c);
                }
                interp = Rc::clone(&self.named_interpreters.initial);
            }
            Operation::Deepquote(s) => {
                for c in ['\'', s] {
                    self.push(c);
                }
                interp = Rc::clone(&self.named_interpreters.initial);
            }
            Operation::Custom(custom) => {
                for c in custom.definition.chars() {
                    self.push(c);
                }
                interp = Rc::clone(&custom.context);
            }
        }
        self.push(']');
        self.push(interp);
    }
    pub fn debug_stack_contents(&mut self) {
        let mut s = String::new();
        for obj in &self.stack {
            if !s.is_empty() {
                s.push(' ');
            }
            obj.show(&mut s, &self.named_interpreters);
        }
        eprintln!("{}", s);
    }
    fn write_symbol(&mut self, symbol: Symbol) -> Result<(), String> {
        let buf = &mut [0u8; 4];
        let buf = symbol.encode_utf8(buf).as_bytes();
        match self.output.write(buf) {
            Ok(_size) => Ok(()),
            Err(e) => Err(e.to_string()),
        }
    }
    fn read_symbol(&mut self) -> Result<Symbol, String> {
        match self.input.next() {
            None => return Err("End of input".to_owned()),
            Some(Err(e)) => return Err(e.to_string()),
            Some(Ok(byte)) => Ok(byte as char),
        }
    }
}

fn show_symbol(s: Symbol, out: &mut String) {
    if s == '\n' {
        out.push_str("\\n")
    } else {
        out.push('\'');
        out.push(s);
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
    fn uniform(oper: Operation) -> Self {
        Self {
            parent: None,
            dict: HashMap::new(),
            archetype: InterpreterArchetype::Other,
            fallback: InterpreterFallback::Uniform(oper),
        }
    }
    fn null() -> Self {
        // there is no need to identify this as the null interpreter,
        // because any interpreter whose fallback is `Illegal` can be
        // regarded as having the null interpreter as its archetype
        Interpreter::uniform(Operation::Builtin(BuiltinOperation::Illegal))
    }
    fn initial() -> Self {
        Self {
            parent: None,
            dict: initial_dict(),
            archetype: InterpreterArchetype::Initial,
            fallback: InterpreterFallback::Uniform(Operation::Builtin(BuiltinOperation::Nop)),
        }
    }
    fn quote(original: Rc<Self>) -> Self {
        Self {
            parent: Some(original),
            dict: HashMap::new(),
            archetype: InterpreterArchetype::Other,
            fallback: InterpreterFallback::Quotesym,
        }
    }
    fn deep_quote() -> Self {
        Self {
            parent: None,
            dict: HashMap::new(),
            archetype: InterpreterArchetype::Other,
            fallback: InterpreterFallback::Deepquote,
        }
    }
    fn get_parent<'a>(&'a self, ctx: &'a InterpretersCtx) -> &'a Rc<Self> {
        self.parent.as_ref().unwrap_or_else(|| &ctx.null)
    }
    fn set_parent(original: Rc<Self>, parent: Rc<Self>, ctx: &InterpretersCtx) -> Rc<Self> {
        if Rc::ptr_eq(original.get_parent(ctx), &parent) {
            return original;
        }
        // Modify the interpreter in-place if possible; otherwise, clone its
        // other properties and make a new one.
        Rc::new(match Rc::try_unwrap(original) {
            Ok(mut i) => {
                i.parent = Some(parent);
                i
            }
            Err(original) => Interpreter {
                parent: Some(parent),
                dict: original.dict.clone(),
                archetype: original.archetype.clone(),
                fallback: original.fallback.clone(),
            },
        })
    }
    fn clear_parent(original: Rc<Self>) -> Rc<Self> {
        if original.parent.is_none() {
            return original;
        }
        // Modify the interpreter in-place if possible; otherwise, clone its
        // other properties and make a new one.
        Rc::new(match Rc::try_unwrap(original) {
            Ok(mut i) => {
                i.parent = None;
                i
            }
            Err(original) => Interpreter {
                parent: None,
                dict: original.dict.clone(),
                archetype: original.archetype.clone(),
                fallback: original.fallback.clone(),
            },
        })
    }
    fn get_action(&self, s: Symbol) -> Operation {
        self.dict
            .get(&s)
            .cloned()
            .unwrap_or_else(|| self.fallback.get_operation(s))
    }
    fn set_action(original: Rc<Self>, s: Symbol, oper: Operation) -> Rc<Self> {
        let mut new_interpreter = match Rc::try_unwrap(original) {
            Ok(i) => i,
            Err(original) => (*original).clone(),
        };
        new_interpreter.dict.insert(s, oper);
        Rc::new(new_interpreter)
    }
    fn show(&self, out: &mut String, ctx: &InterpretersCtx) {
        out.push('(');
        let mut precursor = None;
        // base interpreter
        match (&self.archetype, &self.fallback) {
            (InterpreterArchetype::Initial, _) => {
                precursor = Some(&ctx.initial);
                out.push('v');
            }
            (_, InterpreterFallback::Uniform(Operation::Builtin(BuiltinOperation::Illegal))) => {
                out.push('0')
            }
            (_, InterpreterFallback::Uniform(ref oper)) => {
                oper.show(out, ctx);
                out.push('1');
            }
            // TODO: get good string representations for these interpreters.
            // Given only the currently available instructions, it's
            // impossible to get them on the stack, so it's not that important.
            (_, InterpreterFallback::Quotesym) => out.push_str("<quotesym>"),
            (_, InterpreterFallback::Deepquote) => out.push_str("<deepquote>"),
        }
        for (s, oper) in self.dict.iter() {
            if precursor.map_or(false, |p| p.dict.get(s) == Some(oper)) {
                // no need to include this operation
                continue;
            }
            out.push(' ');
            oper.show(out, ctx);
            show_symbol(*s, out);
            out.push('<');
            oper.show(out, ctx);
        }
        out.push(')');
    }
}

impl InterpreterFallback {
    fn get_operation(&self, s: Symbol) -> Operation {
        match self {
            Self::Uniform(oper) => oper.clone(),
            Self::Quotesym => Operation::Quotesym(s),
            Self::Deepquote => Operation::Deepquote(s),
        }
    }
}

impl Operation {
    fn show(&self, out: &mut String, ctx: &InterpretersCtx) {
        out.push('(');
        match *self {
            Self::Builtin(b) => b.show(out),
            Self::Quotesym(s) => {
                out.push('[');
                show_symbol(s, out);
                out.push_str("]v*");
            }
            Self::Deepquote(s) => {
                // TODO: figure out a better way to represent this
                out.push_str("<deepquote-");
                out.push(s);
                out.push('>');
            }
            Self::Custom(ref custom) => {
                out.push('[');
                out.push_str(&custom.definition);
                out.push(']');
                custom.context.show(out, ctx);
                out.push('*');
            }
        }
        out.push(')');
    }
}

impl BuiltinOperation {
    fn show(self, out: &mut String) {
        out.push_str(match self {
            Self::Nop => "[]v*",
            Self::Reify => "v('v)>",
            Self::Deify => "v('^)>",
            Self::Extract => "v('>)>",
            Self::Install => "v('<)>",
            Self::GetParent => "v('{)>",
            Self::SetParent => "v('})>",
            Self::Create => "v('*)>",
            Self::Expand => "v('@)>",
            Self::Perform => "v('!)>",
            Self::Null => "v('0)>",
            Self::Uniform => "v('1)>",
            Self::Deepquote => "v('[)>",
            Self::Quotesym => "v('')>",
            Self::Output => "v('.)>",
            Self::Input => "v(',)>",
            Self::Dup => "v(':)>",
            Self::Pop => "v('$)>",
            Self::Swap => "v('/)>",
            Self::Illegal => "0('0)>",
        })
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

    fn show(&self, out: &mut String, ctx: &InterpretersCtx) {
        match self {
            Object::Symbol(s) => show_symbol(*s, out),
            Object::Operation(oper) => oper.show(out, ctx),
            Object::Interpreter(int) => int.show(out, ctx),
        }
    }
}
