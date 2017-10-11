# Dreaderef

Dreaderef is a self-modifying esoteric programming language.

## Program State

The execution state of a Dreaderef program consists of a tape of unbounded
signed integers that extends infinitely to the left and right. Each
location on the tape has an index, which is represented by an integer.

In this specification, let `n` represent the literal value `n`, `[n]` represent
the value of the cell at index `n` on the tape, `[[n]]` represent the value of
the cell at index `[n]` on the tape, etc.

A Dreaderef program is a list of integers which represents the initial
contents of this tape. At the beginning of execution, all the integers
in the program are written to the tape, starting at index 0 and extending
to the right. All other locations on the tape are intialized with 0.

To get started, here's an example Dreaderef that computes 1 + 1:

    ; Compute 1 + 1, store in location 5
    add 1 1 5
    ; Output as number
    numo ?

A few things to notice:

- Dreaderef has a preprocessor that removes comments from the program. It
  also recognizes `add`, `numo`, and `?` as shortcuts that represent values.
  The same program could have been written like this:
  
      2 1 1 5 5 0

- Most commands in Dreaderef only take literal arguments. This means that
  if you want the result of a computation to affect other commands, you
  have to use self-modification to splice it into their arguments. In fact,
  the `?` is used to communicate that the memory cell is "missing" and its
  value will be written later.

- The `add` command takes an argument naming where the result of the addition
  will be stored. In this case, it stores it at position `5`, which is the
  argument of `numo`.
  
Here is a reference for all seven commands:

| Name    | Alias | Description
| ------- | ----- | - |
| `end`   | `0`   | Stop execution of the program. Not present in the example because the program is padded with `0`s anyways.
| `deref` | `1`   | Take 2 arguments, `a` and `b`. Store `[a]` (the value pointed to by `a`) in the location `b`.
| `add`   | `2`   | Take 3 arguments. Add `a` and `b` and store the result in location `c`.
| `mul`   | `3`   | Take 3 arguments. Store `a * b` in location `c`.
| `bool`  | `4`   | Take 2 arguments. If `a == 0`, store `0` in location `c`. Otherwise, store `1`.
| `numo`  | `5`   | Take 1 argument and write it as a number to STDOUT.
| `chro`  | `6`   | Take 1 argument, convert it to a character using Python's `chr`, and print the result to STDOUT.
| `chri`  | `7`   | Take 1 argument. Read one character from STDIN, and store the `ord` in STDOUT. Store `0` for EOF.

In Python:

    end:        halt()
    deref a b:  mem[b] = mem[a]
    add a b c:  mem[c] = a + b
    mul a b c:  mem[c] = a * b
    bool a b:   mem[b] = 1 if a else 0
    numo a:     print(a, end="")
    chro a:     print(chr(a), end="")
    chri a:     mem[a] = ord(getchar())

Dreaderef doesn't have a `numi` instruction, but the preprocessor will
replace `*` with an integer from the command-line arguments.

## Control Flow

Position `-1` represents Dreaderef's instruction pointer. It can be written
to and read from like any cell, but it is automatically incremented after
every command, and writing to it causes a jump.

A few things to note:

- Instructions with arguments take up multiple spaces. Each argument occupies
  its own cell.
- By the time a `deref` instruction executes the derefence, the instruction
  pointer is already pointing past it. So the following program:
  
      deref -1 4
      numo ?

  prints `3`.

If Dreaderef encounters an unrecognized instruction, it will ignore it.

## The Preprocessor

Dreaderef's preprocessor knows how to do the following things:

- Remove all text after a `;` on a line.
- Remove all text before a `.` on a line (`;` takes precedence). This is
  intended to allow lines to be numbered.
- Strip unnecessary whitespace.
- Handle `?`, representing a cell whose value is not known statically. Reading
  from such a cell before it has been written is implementation-defined behavior.
  The provided interpreter treats it as `0`.
- Handle `*`, which is replaced with an integer read from the command-line arguments.
- Expand string literals. String literals are delimited by `"` and cannot be continued
  onto multiple lines. The shortcuts `\\`, `\"`, `\n`, `\t`, and `\r` are supported
  in string literals.

## Using the Interpreter

The interpreter is written in Python 3. It can be run from the command
line as:

    $ python3 interpreter.py code.dref [args for the preprocessor]

The interpreter also exports two classes, `Preprocessor` and `Interpreter`:

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
26 in the code, and continues until a 0 is encountered.

    CODE.
    ; Dereference the string pointer
    0.  deref 24 4
    ; End the program if the value pointed to is zero
    3.  deref ? 7
    6.  bool ? 11
    9.  mul -1 ? 13
    13. ? ; This will either be `end` or -1, which is a nop.
    ; Otherwise, output the value
    14. deref 7 18
    17. chro ?
    ; Increment the string pointer
    19. deref 24 24
    22. add 1 29 24
    ; Go back to the beginning
    26. deref 13 -1

    DATA.
    29. "Hello, World!\n"

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
