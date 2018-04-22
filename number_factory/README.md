# Number Factory

A Python implementation of the esoteric programming language
[Number Factory](https://esolangs.org/wiki/Number_Factory).

I have attempted to be as close to spec-compliant as possible, though I made a
few assumptions:

- Numbers are signed, unbounded integers (in other words, Python `int`s).
- Unrecognized commands are ignored.
- The postal office outputs characters without a separator.
- The shipping dock outputs a newline after numbers (although it seems that the
  original interpreter prints a newline instead).
- The receiving dock expects numbers to be on their own line, and ignores lines
  that cannot be parsed as numbers. EOF returns -1.

# Using the Interpreter

The file `src/interpreter.py` is a pure Python 3 implementation of the language.
It has no external dependencies other than the Python standard library.

To interpret a program, run it with the program file as a command-line argument.
Running it with no arguments will cause it to read the program from STDIN.

The file `src/test.py` contains tests for most of the functionality in the
language.

The interpreter can also be used as a module:

- The class `interpreter.State` represents all state held by a Number Factory
  program (the position and state of the robot and rooms).
- The function `interpreter.parse(code)` can be used to parse Number Factory
  code. It raises `interpreter.ParseError` if parentheses are unmatched, or
  otherwise it returns a list of command objects. You can run a command object
  on a given state using `command.run_with(state)`.
- The function `interpreter.run_from_string(code, state)` runs `code` on the
  given state object.
- The function `interpreter.run_from_file(filename)` runs the contents of the
  file at `filename` with a fresh state.

You can also monkey-patch I/O by assigning to the following functions:

- `interpreter.output_fn(i)` should output a number. By default, it prints the
  number to STDOUT.
- `interpreter.input_fn()` should read a line and return it. By default, it
  gets this line from STDIN.
- `interpreter.read_input_integer()` should return an integer. By default, it
  keeps calling `interpreter.input_fn()` until it returns a line parsable as an
  integer, and returns `-1` if it throws `EOFError` or `StopIteration`.
  
# Turing-Completeness

The program `src/brainfuck.py` will convert signed unbounded-cell double-ended
numeric-IO [brainfuck](https://esolangs.org/wiki/brainfuck) programs into
Number Factory, thus demonstrating the Turing-completeness of the latter. Note
that it will generate invalid Number Factory programs (i.e. those with
unmatched parentheses) if the brainfuck program contains unmatched loops.

The converter should take the brainfuck program's filename as an argument. If
no argument is provided, it will be taken from STDIN instead. You can also pass
`-c code` to convert directly from `code` rather than taking a filename.