import random

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

class Number:
    def parse(state, x):
        if isinstance(x, list):
            return len(x)
        elif isinstance(x, str):
            try:
                return int(x)
            except ValueError:
                state.error("Could not parse {:r} as integer".foramt(x))
        elif isinstance(x, int):
            return x
        else:
            state.error("Could not cast {} to number.".format(type(x).__name__))

class String:
    def parse(state, x):
        if isinstance(x, list):
            return "".join(String.parse(i) for i in list)
        elif isinstance(x, str):
            return x
        elif isinstance(x, int):
            return str(x)
        else:
            state.error("Could not cast {} to string.".format(type(x).__name__))

class Block:
    def parse(state, x):
        if isinstance(x, list):
            return x
        elif isinstance(x, str):
            return list(x)
        elif isinstance(x, int):
            return list(range(x))
        else:
            state.error("Could not cast {} to block.".format(type(x).__name__))

@core("+")
def add(state):
    a = state.stack.pop(coerce=Number.parse)
    b = state.stack.pop(coerce=Number.parse)
    state.stack.push(a + b)

@core("*")
def multiply(state):
    a = state.stack.pop(coerce=Number.parse)
    b = state.stack.pop(coerce=Number.parse)
    state.stack.push(a * b)

@core("++")
def concat(state):
    a = state.stack.pop(coerce=Block.parse)
    b = state.stack.pop(coerce=Block.parse)
    state.stack.push(b + a)

@core("{_}")
def wrap(state):
    a = state.stack.pop()
    state.stack.push([a])

alias("_}", "{_} ++")
alias("{_", "// {_} // ++")

@core("//")
def swap(state):
    a = state.stack.pop()
    b = state.stack.pop()
    state.stack.push(a)
    state.stack.push(b)

alias("{_'", "// {_")
    
@core("@")
def mirror(state):
    a = state.stack.pop()
    b = state.stack.pop()
    c = state.stack.pop()
    state.stack.push(a)
    state.stack.push(b)
    state.stack.push(c)

@core("{#}")
def wrap_n(state):
    n = state.stack.pop()
    items = state.stack.pop_many(n)
    state.stack.push(items)

@core("~")
def run(state):
    a = state.stack.pop(coerce=Block.parse)
    state.add_frame(a)

@core("~*")
def run_inline(state):
    a = state.stack.pop(coerce=Block.parse)
    state.extend_frame(0, a)
    
@core(",@")
def read_self(state):
    a = state.pop_from_frame(1)
    state.stack.push(a)

alias("'", ",@")

@core('."')
def output_str(state):
    a = state.stack.pop(coerce=String.parse)
    state.output(a)

alias(".", '` ."')
alias(".#", '>" ."')
alias("<", '." \'|\n| ."')
alias("<#", '>" <')
alias(".'", ',@ ."')
alias("<'", ",@ <")

@core(".@")
def write_self(state):
    a = state.stack.pop()
    state.push_to_frame(1, a)

@core(".@*")
def write_self_list(state):
    a = state.stack.pop(coerce=Block.parse)
    state.extend_frame(1, a)
    
@core(">#")
def to_num(state):
    a = state.stack.pop(coerce=String.parse)
    try:
        a = int(a)
    except ValueError:
        state.error("String {} cannot be converted to integer".format(repr(a)))
    state.stack.push(a)

alias("#", ",@ >#")

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
    a = state.stack.pop(coerce=Number.parse)
    state.stack.push(str(a))

@core("=.")
def define_op(state):
    a = state.stack.pop(coerce=String.parse)
    b = state.stack.pop(coerce=Block.parse)
    bootstrapped[a] = b

@core(",")
def read_char(state):
    a = state.input_parser.read_char()
    state.stack.push(a)

@core(",*")
def read_all(state):
    a = state.input_parser.read_rest()
    state.stack.push(a)

@core(",@*")
def current_frame(state):
    a = state.get_frame_copy(1)
    state.stack.push(a)

alias(",@*'", ",@*")

@core(";#")
def interpret_semicolon_hash(state):
    a = state.input_parser.read_int()
    state.stack.push(a)
    
@core("`")
def escape(state):
    def string_repr(obj):
        if isinstance(obj, list):
            return "[" + " ".join(string_repr(i) for i in obj) + "]"
        else:
            obj = String.parse(state, obj)
            if any((i in obj) for i in "|[] \n\t"):
                return "|{}|".format(obj.replace("\\", "\\\\").replace("|", "\\|"))
            else:
                return obj
    a = state.stack.pop()
    state.stack.push(string_repr(a))

@core("`*")
def pushify(state):
    a = state.stack.pop()
    if isinstance(a, str):
        state.stack.push(["'", a])
    else:
        state.stack.push([a])

alias("->", "`* ,@ =.")

@core(":")
def dup(state):
    a = state.stack.pop()
    state.stack.push(a)
    state.stack.push(a)

@core("_~")
def splat(state):
    items = state.stack.pop(coerce=Block.parse)
    state.stack.push_many(items)

@core("-*")
def negate(state):
    a = state.stack.pop(coerce=Number.parse)
    state.stack.push(-a)

@core("-%")
def reverse(state):
    a = state.stack.pop(coerce=Block.parse)
    state.stack.push(a[::-1])

@core("!;")
def delete(state):
    state.stack.pop()

@core(")")
def pop_end(state):
    *a, b = state.stack.pop(coerce=Block.parse)
    state.stack.push(a)
    state.stack.push(b)

@core("(")
def pop_start(state):
    a, *b = state.stack.pop(coerce=Block.parse)
    state.stack.push(b)
    state.stack.push(a)

alias("!)", ") !;")
alias("(!", "( !;")
alias("(!)", "(! !)")
    
alias("+1", "1 +")
alias("-", "-* +")
alias("--", "1 -")

alias("<->", ": !) -% ++")
alias(">-<", "-% <->")

@core(":%")
def map(state):
    code = state.stack.pop(coerce=Block.parse)
    items = state.stack.pop(coerce=Block.parse)
    results = []
    for i in items:
        state.stack.push(i)
        state.run(code)
        results.append(state.stack.pop())
    state.stack.push(results)

@core(":/")
def splat(state):
    code = state.stack.pop(coerce=Block.parse)
    items = state.stack.pop(coerce=Block.parse)
    for i in items:
        state.stack.push(i)
        state.run(code)

@core("..")
def range_(state):
    a = state.stack.pop(coerce=Number.parse)
    state.stack.push(list(range(a)))

@core("..#")
def length(state):
    a = state.stack.pop(coerce=Block.parse)
    state.stack.push(len(a))

alias(":#", ": ..#")
    
alias(".-", ".. -%")
alias(",,", "2 {#}")

@core("_%_")
def zip_with(state):
    code = state.stack.pop(coerce=Block.parse)
    left = state.stack.pop(coerce=Block.parse)
    right = state.stack.pop(coerce=Block.parse)
    min_len = min(len(left), len(right))
    rest = left[min_len:] or right[min_len:]
    result = []
    for i in range(min_len):
        state.stack.push(right[i])
        state.stack.push(left[i])
        state.run(code)
        result.append(state.stack.pop())
    result.extend(rest) # add all unused elements
    state.stack.push(result)

# enumerate
alias("#,_", ":# .. // [,,] _%_")

@core("%")
def modulo(state):
    a = state.stack.pop(coerce=Number.parse)
    b = state.stack.pop(coerce=Number.parse)
    state.stack.push(b % a)

@core("/")
def int_divide(state):
    a = state.stack.pop(coerce=Number.parse)
    b = state.stack.pop(coerce=Number.parse)
    state.stack.push(b // a)

@core("~#~")
def run_both(state):
    amount = state.stack.pop(coerce=Number.parse)
    block_2 = state.stack.pop(coerce=Block.parse)
    block_1 = state.stack.pop(coerce=Block.parse)
    items = state.stack.pop_many(amount)
    
    state.stack.push_many(items)
    state.run(block_1)
    state.stack.push_many(items)
    state.run(block_2)

# divmod
alias("/%", "[/] [%] 2 ~#~")

@core("#/")
def divisors(state):
    a = state.stack.pop(coerce=Number.parse)
    result = [i for i in range(1, a+1) if a % i == 0]
    state.stack.push(result)

@core("%=")
def divisible(state):
    b = state.stack.pop(coerce=Number.parse)
    a = state.stack.push(coerce=Number.parse)
    result = int(a % b == 0)
    state.stack.push(result)

@core("=")
def equiv(state):
    a = state.stack.pop()
    b = state.stack.pop()
    result = int(a == b)
    state.stack.push(result)

# count divisors
alias("%:#", "#/ ..#")
# prime - I don't even care about efficiency
alias("#.%", "%:# 2 =")

@core("?#")
def random_number(state):
    a = state.stack.pop(coerce=Number.parse)
    if a < 1:
        state.error("Cannot get random value with max < 1")
    state.stack.push(random.randrange(a))

@core("?=")
def choice(state):
    a = state.stack.pop(coerce=Block.parse)
    if not a:
        state.error("Choose from empty list")
    state.stack.push(random.choice(a))

# get random bit
alias("%?", "2 ?#")

# 2x+1 and (x-1)/2 respectively
alias("*)", "2 * +1")
alias("(*", "-- 2 /")

@core("=#")
def get_array_item(state):
    a = state.stack.pop(coerce=Number.parse)
    b = state.stack.pop(coerce=Block.parse)
    a %= len(b)
    state.stack.push(b[a])

# modulo 2
alias("2%", "2 %")

@core("+/")
def sum(state):
    a = state.stack.pop(coerce=Block.parse)
    result = 0
    for i in a:
        result += Number.parse(state, i)
    state.stack.push(result)

@core("*//")
def product(state):
    a = state.stack.pop(coerce=Block.parse)
    result = 1
    for i in a:
        result *= Number.parse(state, i)
    state.stack.push(result)

alias("!*", ".. [+1] :% *//")

@core(" ")
def space(state):
    state.stack.push(" ")
@core("\n")
def newline(state):
    state.stack.push("\n")

@core("=/")
def split_newlines(state):
    a = state.stack.pop(coerce=String.parse)
    state.stack.push(a.split("\n"))
@core("*/")
def join_newlines(state):
    a = state.stack.pop(coerce=Block.parse)
    state.stack.push("\n".join(String.parse(state, i) for i in a))
@core("/-\\")
def ignore_top(state):
    code = state.stack.pop(coerce=Block.parse)
    a = state.stack.pop()
    state.run(code)
    state.stack.push(a)
@core(r"/#\\'")
def ignore_top_n(state):
    n = state.stack.pop(coerce=Number.parse)
    code = state.stack.pop(coerce=Block.parse)
    top_n = state.stack.pop_many(n)
    state.run(code)
# ignore top n, where n is a literal
alias("/#\\", ",@ ># /#\'")

# with-lines
alias("{/}", "' =/ {_' ' */ _}")

@core('"/')
def fracture(state):
    a = state.stack.pop(coerce=String.parse)
    state.stack.push(list(a))
@core('"*')
def weld(state):
    a = state.stack.pop(coerce=Block.parse)
    state.stack.push("".join(String.parse(i) for i in a))

# with-chars
alias('{"}', """ ' "/ {_' ' "* _} """)

@core("{-")
def comment(state):
    while state.get_frame(0) and state.pop_from_frame(0) != "-}":
        pass

# Single comment
alias("##", ",@ !;")