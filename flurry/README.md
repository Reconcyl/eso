# Flurry

This is a Haskell implementation of the Flurry esoteric programming language. Flurry is a stack-based functional language with heavy inspiration from [Brain-Flak](https://github.com/DJMcMayhem/Brain-Flak).

## Description

The main data type in Flurry is the *expr*. An *expr* is a function that takes a single input of type *expr* and produces an output of type *expr*. This function may modify the stack in the process (you can think of it as  `(expr, stack) -> (expr, stack)` if you prefer).

The stack contains values of type *expr*. Attempting to pop from an empty stack will instead return the `I` combinator (`λx. x`).

The only syntactically relevant characters in a Flurry program are `()[]{}<>`, and they must always be balanced. These expressions evaluate to an *expr*, and possibly modify the stack.

Here are the four nilads:

- `[]` evaluates to the height of the stack as a Church numeral.
- `()` evaluates to the `K` combinator (`λxy. x`).
- `<>` evaluates to the `S` combinator (`λxyz. xz(yz)`).
- `{}` pops the stack.

Here are the four monads:

- `[...]` does nothing and evaluates to `...` (acting as a grouping operator).
- `(...)` pushes `...` to the stack, and also returns it.
- `<abc>` evaluates to `a ￮ b ￮ c` (where `￮`  represents function composition).
- `{...}` returns a function that evaluates `...` with its argument on top of the stack (so `{{}}` is the identity function, for example).

Input and output are encoded as Church numerals on the stack. If any stack elements are not Church numerals, they are ignored.

## Snippets

Duplicate the top stack value:

    (({}))

Return the iota combinator (`λx. xSK`):

    {{}<>()}

Return the sum of the top two stack values (assuming they are Church numerals):

    {}[<><<>()>]{}

Return the product of the top two stack values (assuming they are Church numerals):

    <{}{}>

## Using the interpreter

You can compile the interpreter with

    $ ghc -o target/Flurry -outputdir target src/*.hs

This will create a directory called `target` containing an executable called `Flurry`. You can run a program with:

    $ echo "(<{}{}>)" > code.flr
    $ target/Flurry -inn code.flr 10 20
    200

You can also pass a program directly:

    $ target/Flurry -inn -c "(<><<>()>({}))" 99
    99 100

The first argument (`-inn` in this example) specifies IO behavior with 3 characters:

- The first character describes what happens to the stack after the program is finished:
  - `i` means to output them as integer values.
  - `b` means to output them as byte values.
  - `d` means to output them as integer values to STDERR.
  - `n` means to ignore them.
- The second character describes what happens to the return value of the program:
  - `i` means to output it as an integer value.
  - `d` means to output it as an integer value to STDERR.
  - `n` means to ignore it.
- The third character describes how STDIN is used to initialize the stack:
  - `i` means the contents of STDIN are interpreted as integers.
  - `b` means the contents of STDIN are interpreted as their byte values.
  - `n` means the contents of STDIN is ignored.
