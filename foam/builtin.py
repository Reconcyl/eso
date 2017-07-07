import parse

core_ops = {}
bootstrapped = {}

def core(name):
    def decorate(func):
        if name in core_ops or name in bootstrapped:
            raise RuntimeError("Error: the name {} is already defined".format(repr(name)))
        core_ops[name] = func
    return decorate

def alias(name, operation):
    if name in core_ops or name in bootstrapped:
        raise RuntimeError("Error: the name {} is already defined".format(repr(name)))
    # I don't pass a FoamState object to parse.parse() like I do when I
    # parse the user's code, because I don't have it. Instead, I just pass
    # an object().
    # All strings passed to parse.parse() here are just hardcoded string
    # literals, so parsing should never fail.
    # If it does fail, it will blow up in my face, so I will have an
    # incentive to fix it as quickly as possible.
    bootstrapped[name] = parse.parse(object(), operation)

@core("+")
def add(state):
    "Pops two numbers from the stack and adds them."
    a = state.stack.pop(expect=int)
    b = state.stack.pop(expect=int)
    state.stack.push(a + b)

@core("++")
def concat(state):
    "Pops two lists from the stack and concatenates them."
    a = state.stack.pop(expect=list)
    b = state.stack.pop(expect=list)
    state.stack.push(b + a)

@core("{_}")
def wrap(state):
    "Pops and object and pushes it, wrapped in a list."
    a = state.stack.pop()
    state.stack.push([a])

# appends an item to the the end of a list
alias("_}", "{_} ++")
# prepends the item at the beginning of the list to the end of the list
alias("{_", "// {_} // ++")

@core("//")
def swap(state):
    "Swaps the top two stack values."
    a = state.stack.pop()
    b = state.stack.pop()
    state.stack.push(a)
    state.stack.push(b)

# like {_, but swaps its args first
alias("{_'", "// {_")
    
@core("@")
def mirror(state):
    "Reverses the top three stack values: a, b, c -> c, b, a"
    a = state.stack.pop()
    b = state.stack.pop()
    c = state.stack.pop()
    state.stack.push(a)
    state.stack.push(b)
    state.stack.push(c)

@core("{#}")
def wrap_n(state):
    "Pops a number `n` and pushes a list consisting of the first `n` stack values."
    n = state.stack.pop(expect=int)
    result = []
    for _ in range(n):
        result.append(state.stack.pop())
    state.stack.push(result[::-1])
    
@core("~")
def run(state):
    "Pops a list and inserts it as a new code frame, where it will be immediately executed."
    a = state.stack.pop(expect=list)
    state.add_frame(a)

@core("~*")
def run_inline(state):
    """Like `~`, but does not create a new frame, and instead adds to the previous one.
       This means that it cannot be accessed by self-modification commands like `,@`, etc."""
    a = state.stack.pop(expect=list)
    state.extend_frame(0, a)
    
@core(",@")
def read_self(state):
    "Reads a token from the second-to-last code frame (i.e. not the one that called it)"
    a = state.pop_from_frame(1)
    state.stack.push(a)

# Used for string literals, i.e. `' hi` will push "hi" to the stack.
# This works because aliasing defines a function that will create a code frame
# when it is called, so ,@ will read from the code frame that it was called in.
alias("'", ",@")

@core('."')
def output_str(state):
    "Output a string."
    a = state.stack.pop(expect=str)
    state.output(a)

# output string representation
alias(".", '` ."')
# output numeric
alias(".#", '>" .')
# output with trailing newline
alias("<", '." \'|\n| ."')
# output string literal
alias(".'", ',@ ."')
# output string literal with newline
alias("<'", ",@ <")

@core(".@")
def write_self(state):
    "Pops an object and inserts it at the second-to-last code frame."
    a = state.stack.pop()
    state.push_to_frame(1, a)

@core(".@*")
def write_self_list(state):
    "Like `.@`, but writes an entire block rather than a single token."
    a = state.stack.pop(expect=list)
    state.extend_frame(1, a)
    
@core(">#")
def to_num(state):
    "Pops a string and converts it to an integer."
    a = state.stack.pop(expect=str)
    try:
        a = int(a)
    except ValueError:
        state.error("String {} cannot be converted to integer".format(repr(a)))
    state.stack.push(a)

 # this is for easily creating numeric literals, i.e. # 10
alias("#", ",@ >#")

# shorthands for some numeric literals (more will be added)
alias("0", "# 0")
alias("1", "# 1")
alias("2", "# 2")
alias("-1", "# -1")
alias("10", "# 10")
alias("#8", "# 256")
alias("e2", "# 100")
alias("e3", "# 1000")

@core('>"')
def to_str(state):
    "Pops a number and converts it to a string."
    a = state.stack.pop(expect=int)
    state.stack.push(str(a))

@core("=.")
def define_op(state):
    "Pops a name and a block and creates a new builtin with that name."
    a = state.stack.pop(expect=str)
    b = state.stack.pop(expect=list)
    bootstrapped[a] = b

@core(",")
def read_char(state):
    "Reads a character from input."
    a = state.input_parser.read_char()
    state.stack.push(a)

@core(",*")
def read_all(state):
    "Reads the rest of the input."
    a = state.input_parser.read_rest()
    state.stack.push(a)

@core(",@*")
def current_frame(state):
    "Gets the frame before the calling frame."
    a = state.get_frame_copy(1)
    state.stack.push(a)

# Gets the current frame. For cheating quines, or recursion.
alias(",@*'", ",@*")

@core(";#")
def interpret_semicolon_hash(state):
    "Reads a number."
    a = state.input_parser.read_int()
    state.stack.push(a)
    
@core("`")
def escape(state):
    "Gets the string representation of a string or list."
    def string_repr(obj):
        if isinstance(obj, str):
            if any((i in obj) for i in "|[] \n\t"):
                result = "|{}|".format(obj.replace("\\", "\\\\").replace("|", "\\|"))
            else:
                result = obj
            return result
        elif isinstance(obj, list):
            return "[" + " ".join(string_repr(i) for i in obj) + "]"
        else:
            state.error("Type {} cannot be escaped".format(type(obj).__name__))
    a = state.stack.pop()
    state.stack.push(string_repr(a))

@core("`*")
def pushify(state):
    """Pops a value and pushes a block that, when executed, pushes the original
       value onto the stack. Will break if ' is redefined."""
    a = state.stack.pop()
    if isinstance(a, str):
        state.stack.push(["'", a])
    else:
        state.stack.push([a])

# assigns a value to a variable, i.e. 0 -> k
alias("->", "`* ,@ =.")

@core(":")
def dup(state):
    "Pops a value and pushes it back twice."
    a = state.stack.pop()
    state.stack.push(a)
    state.stack.push(a)

@core("_~")
def splat(state):
    "Dumps the items in a list to the stack."
    # fix later