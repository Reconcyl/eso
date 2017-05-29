"""
Defines the operations for the linear mode.
>   | push 0 to the stack
0-9 | multiply by 10 and add the corresponding digit
+   | add / join arrays
-   | subtract
_   | negate / reverse array
*   | multiply
$   | divide by 2
%   | modulo
o   | output character / string
)   | increment
(   | decrement
p   | output number / array (does not pop)
<   | output newline (same as >10p)
!   | boolean NOT
[   | push empty array
]   | wrap in array
#   | switch to 2d mode
?   | switch to 2d mode if nonzero (does not pop)
"   | string literal (array of numbers)
:   | duplicate
\   | swap
@   | rotate
;   | pop n, rotate n items (\ is 2;, @ is 3;, etc.)
'   | like ;, but rotate the other way
d   | delete stack item
.   | make stack into array
&   | unpack array
,   | array length
{   | get array item
r   | pop and store in register
R   | get from register
`   | debug
"""

all_operations = {}
def operator(char):
    def decorate(func):
        all_operations[char] = func
    return decorate

def type_match(objects, types):
    for obj, type_ in zip(objects, types):
        if not isinstance(obj, type_):
            return False
    return True

def error_types(state, c, *args):
    types = [type(arg).__name__ for arg in args]
    state.error("Invalid type for {}: {}".format(c, types))

INTS = (int, int)
LISTS = (list, list)

@operator(">")
def push_0(state):
    state.stack.push(0)

@operator(" ")
def space(state):
    pass

@operator("+")
def plus(state):
    arg1 = state.stack.pop()
    arg2 = state.stack.pop()
    args = (arg1, arg2)
    if type_match(args, INTS) or type_match(args, LISTS):
        state.stack.push(arg2 + arg1)
    else:
        error_types(state, "+", arg1, arg2)

@operator("-")
def minus(state):
    arg1 = state.stack.pop()
    arg2 = state.stack.pop()
    if type_match((arg1, arg2), INTS):
        state.stack.push(arg2 - arg1)
    else:
        error_types(state, "-", arg1, arg2)

@operator("_")
def negate(state):
    a = state.stack.pop()
    if isinstance(a, int):
        state.stack.push(-a)
    else:
        state.stack.push(a[::-1])

@operator("*")
def times(state):
    a = state.stack.pop()
    b = state.stack.pop()
    if type_match((a, b), LISTS):
        error_types(state, "*", a, b)
    else:
        state.stack.push(a * b)

@operator("$")
def div_2(state):
    a = state.stack.pop()
    if isinstance(a, int):
        state.stack.push(a // 2)
    else:
        error_types(state, "$", a)

@operator("%")
def mod(state):
    a = state.stack.pop()
    b = state.stack.pop()
    if type_match((a, b), INTS):
        state.stack.push(b % a)
    else:
        error_types(state, "%", a, b)

@operator("o")
def o(state):
    a = state.stack.pop()
    def stringify(obj):
        if isinstance(obj, int):
            try:
                return chr(obj)
            except ValueError:
                state.error("Invalid character")
        else:
            return "".join(stringify(i) for i in obj)
    state.output_func(stringify(a))

@operator(")")
def inc(state):
    a = state.stack.pop()
    if isinstance(a, int):
        state.stack.push(a + 1)
    else:
        error_types(state, ")", a)

@operator("(")
def dec(state):
    a = state.stack.pop()
    if isinstance(a, int):
        state.stack.push(a - 1)
    else:
        error_types(state, "(", a)

@operator("p")
def p(state):
    a = state.stack.pop()
    state.output_func(a)
    state.stack.push(a)

@operator("<")
def newline(state):
    state.output_func("\n")

@operator("!")
def not_(state):
    a = state.stack.pop()
    state.stack.push(int(not a))

@operator("[")
def empty(state):
    state.stack.push([])

@operator("]")
def wrap(state):
    a = state.stack.pop()
    state.stack.push([a])

@operator("#")
def swap_modes(state):
    state.direction = 1 # Directions.right

@operator("?")
def cond(state):
    a = state.stack.pop()
    state.stack.push(a)
    if a:
        state.direction = 1 # Directions.right

@operator("\"")
def quote(state):
    state.current_str = []

@operator(":")
def dup(state):
    a = state.stack.pop()
    for _ in range(2):
        state.stack.push(a)

@operator("\\")
def swap(state):
    a = state.stack.pop()
    b = state.stack.pop()
    state.stack.push(a)
    state.stack.push(b)

@operator("@")
def rotate(state):
    a = state.stack.pop()
    b = state.stack.pop()
    c = state.stack.pop()
    state.stack.push(b)
    state.stack.push(a)
    state.stack.push(c)

@operator(";")
def rotate_n(state):
    n = state.stack.pop()
    top = [state.stack.pop() for _ in range(n)]
    top_first = top.pop() # get the bottom element
    for i in top[::-1]:
        state.stack.push(i)
    state.stack.push(top_first)

@operator("'")
def rotate_n(state):
    n = state.stack.pop()
    top = [state.stack.pop() for _ in range(n)]
    state.stack.push(top.pop(0))
    for i in top[::-1]:
        state.stack.push(i)

@operator("d")
def delete(state):
    state.stack.pop()

@operator(".")
def array_stack(state):
    original = state.stack.stack
    state.stack.clear()
    state.stack.push(original)

@operator("&")
def unpack(state):
    array = state.stack.pop()
    if isinstance(array, int):
        error_types(state, "&", array)
    for i in array:
        state.stack.push(i)

@operator(",")
def comma(state):
    a = state.stack.pop()
    if isinstance(a, list):
        state.stack.push(len(a))
    else:
        state.stack.push(list(range(a)))

@operator("=")
def get_item(state):
    array = state.stack.pop()
    index = state.stack.pop()
    if type_match((array, index), (list, int)):
        pass
    elif type_match((array, index), (int, list)):
        array, index = index, array
    else:
        error_types(state, "=", array)
    state.stack.push(array[index])

@operator("r")
def store_register(state):
    state.register = state.stack.pop()

@operator("R")
def get_register(state):
    state.stack.push(state.register)

@operator("`")
def debug(state):
    state.output_func("\nDebug: {}\n".format(", ".join(repr(i) for i in state.stack.stack)))

# add in the digits - I'm too lazy to do each one
def digit_factory(digit):
    def func(state):
        a = state.stack.pop()
        if isinstance(a, list):
            error_types(state, str(digit), a)
        state.stack.push(a * 10 + digit)
    return func
for digit in "0123456789":
    operator(digit)(digit_factory(int(digit)))