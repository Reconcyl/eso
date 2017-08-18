# Dreaderef

Dreaderef is a self-modifying esoteric programming language.

## Program State

The execution state of a Dreaderef program consists of a tape of unbounded
signed integers that extends infinitely to the left and right. Each
location on the tape has an index, which is represented by an integer.

In this specification, let `n` represent the literal value `n`, `*n` represent
the value of the cell at index `n` on the tape, `**n` represent the value of
the cell at index `*n` on the tape, etc.

## Program Structure

A Dreaderef program is a list of integers which represents the initial
contents of this tape. At the beginning of execution, all the integers
in the program are written to the tape, starting at index 0 and extending
to the right. All other locations on the tape are intialized with 0.

## Execution

Dreaderef program execution proceeds in ticks. What is executed during
each tick depends on the value of `**-1` (that is, the cell pointed to
by the cell at `-1`, which represents the instruction pointer).

- If `**-1` is 0, then program execution ends.
- If `**-1` is 1, then the `deref` instruction is executed: the instruction
  pointer is incremented and then dereferenced twice to read two arguments,
  `a` and `b`. The cell at location `b` is assigned the value of the
  cell at location `a`. In other words, `*b = *a`.
- If `**-1` is 2, the `add` instruction is executed: the IP is incremented
  and dereferenced three times to read three arguments, `a`, `b`, and
  `c`. `a` and `b` are added together and the result is stored in location
  `c`. In other words, `*c = a + b`.
- If `**-1` is 3, the `mul` instruction is executed: three arguments
  `a b c` are read. `a` and `b` are multiplied together and the result
  is stored in `c`: `*c = a * b`.
- If `**-1` is 4, the `bool` instruction is executed: two arguments
  `a b` are read. If `a` is nonzero, `1` is stored in location `b`.
  Otherwise, `0` is stored instead. In other words, `*b = a?1:0`.
- If `**-1` is 5, the `numo` instruction is executed: one argument
  is read, and it is printed (in its numeric form) to STDOUT, without
  any delimiter.
- If `**-1` is 6, the `chro` instruction is executed: one argument
  `a` is read, and the Unicode character with code-point `a` is written
  to STDOUT. If no such character exists, nothing is printed.
- If `**-1` is 7, the `chri` instruction is executed: one argument `a`
  is read, and a single character is read from STDIN. The Unicode code-point
  of this character is taken and the result is stored in `*a`. If end-of-file
  has been reached, 0 is written instead.
- Any other instruction is simply ignored.

After the instruction is executed, the value of `*-1` is incremented
one more time, so that it falls on the next instruction.

## The Preprocessor

The reference interpreter for Dreaderef has a preprocessor to make creating
programs slightly easier.

Here is an example Dreaderef file that demonstrates some of the preprocessor's
features:

    ; Everything after the first ";" on a line is ignored.
    As is everything before the last ".".
    ; However, semicolons take priority. So this sentence is still commented
    
    0. chro "H"
    2. chro "a"
    4. chro "i"
    6. end
    7. "hmm..." 719 ; This will never be executed.
    
After being fed through the preprocessor, the result is:

    6 72 6 97 6 105 0 104 109 109 46 46 46 719

The preprocessor also provides two special values, `?` and `*`.

- `?` is intended to signal that a location will not be used until it
  is modified later. Accessing a value that was initialized with `?`
  is undefined behavior (although the reference implementation just
  replaces it with a `0`).
- `*` will be replaced with an integer from the command-line arguments.
  It is intended to help write programs that take integer input (since
  there is no `numi` instruction).

## Using the Interpreter

The interpreter is written in Python 3. It can be run from the command
line as:

    $ python3 interpreter.py code.dref [args for the preprocessor]

You can also `import` it and use it as a module from Python code. The
code contains two classes, `Preprocessor` and `Interpreter`.

    Preprocessor(error_callback_function, text, arguments)
        .process() # returns a list of numbers
    
    Interpreter(numbers)
        .run() # run the code

## Contributing

While I welcome suggestions for the language, I intend to keep Dreaderef
as minimalistic as possible, so it is possible they will not be accepted.

However, if you become aware of any bugs, especially in the preprocessor,
please notify me so that I can fix them.

## Example Programs

The following is a trivial "Hello, World!" program:

    chro "H"
    chro "e"
    chro "l"
    chro "l"
    chro "o"
    chro ","
    chro " "
    chro "W"
    chro "o"
    chro "r"
    chro "l"
    chro "d"
    chro "!"
    chro "\n"

The following is an extensible version. It outputs data starting at position
26 in the code, and continuing until a null byte is encountered.

It uses a very hacky trick to do conditional looping in which the cell that
holds the instruction pointer (-1) is actually executed as code. This trick
is used for the sole purpose of minimizing the program's size.

    CODE.
    ; Output the character pointed to by the counter
    0.  deref 25 4
    3.  deref ? 7
    6.  chro ?
    ; Increment the counter
    8.  deref 25 13
    11. add 1 ? 25
    ; Check whether the cell pointed to by the counter is 0
    15. deref 7 19
    18. bool ? 23
    ; Subtract 2 from the boolean value to get either -2 or -1,
    ; and store the result in the instruction pointer
    21. add -2 ? -1
    
    DATA.
    25. 26
    26. "Hello, World!\n"

## Turing Completeness

I do believe Dreaderef is Turing complete. Since cell sizes are unbounded,
it should be possible to simulate a Minsky machine fairly easily.

An interesting problem, then, is minimization. Of course, I/O instructions
could trivially be removed, leaving the language with 5 instructions.
Of those, `bool` and `mul` seem the most eliminable. If this was done,
there would be no good way to check whether a cell was zero unless it
was known to be one of a finite set of values, meaning that any portion
of the tape used for data processing would essentially act like that of
a Turing machine, in which only a fixed alphabet of symbols could be encountered.

This leaves the language with three instructions: `end`, `deref`, and
`add`. Of these, `end` might be removable by changing the language such
that if the instruction pointer ever points to a negative value, execution
terminates.

It is possible that even `add` could be removed, leaving the language
with `deref` as the only instruction. However, doing this would be beyond
my understanding.
