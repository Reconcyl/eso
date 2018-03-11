# The Momema programming language

Momema is a simple prefix-based esolang. The name comes from a comment (I forget where) that the
entirety of programming is "just MOving MEMory Around."

Momema's data storage is a tape of unbounded integers unbounded in both directions. Each cell in
the tape has an *index*, which is associated with an integer.

A Momema program consists of a list of *commands*. Commands are one of the following:

- A jump, which is a string of lowercase letters, followed by an expression.
- An assignment, which consists of two expressions.

Expressions are one of the following:

- A nonnegative integer literal, consisting of one or more digits. This evaluates to itself.
- A negation, which is a `-` followed by an expression. This evaluates to the negation of what its
  argument evaluates to.
- A sum, which is a `+` followed by two expressions. This evaluates to the sum of what its
  arguments evaluate to.
- A tape reference, which is a `*` followed by an expression. This evaluates to the value of the
  cell at the position on the tape given by what its argument evaluates to.
- A normalization, which is `=` followed by an expression. This evaluates to 0 if its argument
  evaluates to 0, and 1 otherwise.

The instructions have the following semantics:

- An assignment evaluates its two expressions. The cell at index *a* on the tape is given value
  *b*, where *a* is the first argument, and *b* is the second.
- A jump first identifies all other jump instructions in the program with the same label. It
  evaluates its argument *n*, and then jumps forward by *n* such instructions. If *n* is zero, no
  action is taken. If *n* is negative, it jumps backwards. Additionally, *n* can go further than
  the end of the program, in which case it "wraps around" to the beginning. In this way, jump
  instructions act as both labels and GOTOs.

A few things to note:

- A `0` in front of a string of digits is parsed separately. For example, `010 003` parses as five
  tokens: `0`, `10`, `0`, `0`, and `3`.
- Spaces, tabs, linefeeds, and parentheses may be used in the program and are ignored (except that
  they force a numeric literal or label to end). `#` can be used to begin a single-line comment,
  and `/` a multi-line comment.

## IO

The addresses `-8` and `-9` are special:

- When a value is read from `-8`, it evaluates to an integer read from standard input. When a value
  is written to `-8`, it is printed to standard output.
- The address `-9` is similar, but it operates with bytes instead of ASCII decimal integers.

Both addresses return -1 for EOF, but since -1 is a valid integer input, you are advised to use
`-8` to check for EOF.

## Using the Interpreter

The interpreter (`Momema.hs`) is written in Haskell. You can compile it with `ghc Momema` (assuming
GHC is installed).

The produced executable `./Momema` should take the file name of the executable, and optional
command-line arguments:

- `-d` activates debug mode, which adds an instruction `!`. `!` prints debug information (the tape
  and position in the program) to STDERR. It also adds an expression form `?`, which takes an
  argument, prints debug information and that argument to STDERR, and then returns that argument.
  (This is useful because expressions in Momema often get very large and it is inconvenient to
  debug them).
- `-i` activates interactive mode. Interactive mode behaves like debug mode as well as adding the
  following features:
  - *Holes*. Holes are written as `_`, and represent unimplemented expressions in the program.
    Every time a hole is evaluated, the user will be asked to fill it in. Holes can optionally be
    given names by appending one or more uppercase letters; if the program attempts to evaluate a
    hole sharing a name with one already evaluated, the value given by the user will be re-used.
  - *Breakpoints*. A `|` instruction is added, which temporarily suspends execution and puts the
    user into a console where they can enter code to be executed and expressions to be evaluated.
    The user can exit the console and resume execution by typing `:quit`, or revert any edits made
    in the console by typing `:revert`. The user will also be sent into the interactive console at
    the end of execution.
- If no program file is given, the interactive console will be launched automatically.

You can find example Momema programs in the `examples/` directory.
