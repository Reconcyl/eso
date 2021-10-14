# Snowmelt

```
snowmelt: ML interpreter for Flurry
Usage: snowmelt [OPTIONS/FILE]...

Options:
    -h
        Print this help message.
    -I{format}, -I={format}
        Set the input format.
    -O{format}, -O={format}
        Set the output format.
    -R{format}, -R={format}
        Set the return value format.
    -m
        Use magic impure functions for IO.
    -c
        Treat the next argument as literal code to be interpreted.
    -f
        Treat the next argument as a file name (the default).
    --
        Treat the next argument as a file name, even if it starts with `-`.
    123
        Push the number to the stack above any input.

Formats:
    i   Church numeral displayed as decimal integer
    b   Church numeral displayed as raw byte         (I/O only)
    u   Church numeral displayed as UTF-8 codepoint  (I/O only)
    d   Debug representation                         (O/R only)
    n   Ignored
```

---

Snowmelt is an interpreter for the [Flurry] programming language which is written
in [Standard ML]. 

It offers a number of improvements over the original Haskell interpreter:

- It can be compiled with the optimizing [MLton] compiler, which provides good
  performance. Almost all of the programs I tested run more quickly in this
  interpreter than they do in the original; however, there do seem to be cases
  where the Haskell interpreter is capable of being more lazy with not computing
  ignored output values.

- It represents terms using their constituent combinators explicitly rather than
  storing terms as opaque functions. This reduces the use of dynamic dispatch,
  and also enables a number of optimizations, such as rewriting `<[() x] f>` to
  `[() x]` when `f` is known to be pure. It also allows integers (represented as
  Church numerals) to be manipulated *much* more efficiently, and these
  manipulations are detected at the point of construction rather than through
  fragile static analysis. So for example, a term which evaluates to `<><<>()>`
  will have its value rewritten to a special `Succ` combinator, which will cause
  expressions like `n Succ m` for Church numeral `n` and `m` to be detected and
  replaced with the Church numeral representing their sum. All of this is
  transparent to the programmer — in particular, we do not assume that these
  optimizations will always apply, so Church-like terms that aren't noticed by
  the optimizer, like `{<({}){}>}`, will still be displayed properly during
  output.

- The command-line interface is different and should hopefully be more sane. The
  same IO format options for input, output, and program return value, exist,
  albeit with slightly different syntax. These formats are also extended with
  support for UTF-8 codepoints and an improved, unambiguous debug representation
  (another thing enabled by the combinator representation of terms).

- Since Standard ML is an impure language, we can represent stack manipulations
  with side effects rather than monads. We can also `-m` flag which will replace
  the normal stack-based IO scheme with a pair of magic, impure combinators
  placed on the stack to perform input and output. This enables Flurry programs
  to be written that use interactive IO.

- By passing multiple arguments, you can split a program into multiple files or
  lines. Executing programs `foo`, `bar`, and `baz` has the same semantics as
  the single program `[foo][bar][baz]`. This can be used for libraries, for
  example, by constructing your program as a function which takes the library as
  an argument, or by constructing the library as a function which exports
  functions on the stack and returns `I`. Note that the same IO configuration
  applies globally for all programs; the presence of a single `-c` argument
  causes a different set of IO defaults to be adopted.

## Deviations

In general, Snowmelt is very true to the semantics of the original interpreter
despite working very differently. There are a few exceptions, however:

- Constructing very large Church numerals can lead to an overflow error, where
  in the Haskell interpreter it would yield a function that, while valid, would
  (practically) hang the program when displayed or applied to anything.
- The function `<>()`, which corresponds to a Church numeral in combinatory
  logic (`S K f x = K f (f x) = x = 0 f x`), is rewritten to the Church numeral
  for zero. This is technically incorrect, because the value `f x` is computed
  and discarded, which is a no-op in combinatory logic but can have side effects
  in Flurry, but I decided that this optimization was worth doing because it's
  so common in Flurry programs to use `<>()` as a Church numeral representation
  of zero. If you really need the original behavior, you can use `<>{(){}}`,
  which isn't rewritten.

## Using the interpreter

A Makefile is provided for compiling the program with MLton.

```
$ make mlton
$ # program credit: hyper-neutrino
$ # -Ob means binary output; -Rn means to ignore the return value
$ ./main -ObRn -c '(){}[(){}[(){}[(){}[(){}[(){}[(){}[(){}(<[{<({}){}>}{<({})({}){}>}][{<({})({}){}>}{<({}){}>}]>)((((<><<>()>[{<({}){}>}<{<({}){}>}{<({})({})({})({}){}>}>])[<><<>()>]{<({})({})({})({})({})({}){}>}))[<><<>()>]{<({})({}){}>})](<{<({})({})({}){}>}[<><<>()><{<({}){}>}{<({})({})({})({}){}>}>]>)]({<({})({})({})({}){}>}{<({}){}>})](<{<({})({}){}>}[<><<>()><{<({})({})({}){}>}{<({})({})({})({})({})({}){}>}>]>)](([{<({}){}>}<{<({}){}>}{<({})({})({})({}){}>}>][<><<>()>][<><<>()>[<{<({}){}>}{<({})({})({})({}){}>}>]])[<><<>()>]{<({})({}){}>})]([{<({}){}>}<{<({}){}>}{<({})({})({})({}){}>}>][<><<>()>][{<({})({}){}>}{<({}){}>}])]({<({}){}>}<{<({}){}>}{<({})({})({})({}){}>}>)](<><<>()>[{<({})({})({})({}){}>}{<({}){}>}])'
Hello, World!
```

It can also compile with [Standard ML of New Jersey]:

```
$ make smlnj
$ # creates a heap image file, whose name varies by target
$ heap_file=( main_smlnj.* )
$ # program credit: me
$ echo '{{}{<{}(<><<>()>{})>}[(<>())()]}{}' >factorial.flurry
$ # without `-c`, the program is run from a file
$ # -Ii means integer input; -On means to ignore the stack for output;
$ # -Ri means to treat the return value as an integer
$ echo 20 | sml @SMLload="$heap_file" -IiOnRi factorial.flurry
2432902008176640000
```

## Interactive IO

Normally, IO works by having input values placed on the stack prior to execution
and then popped after execution. Integers can additionally be placed on the
stack by suppling them as command-line arguments. If the `-m` interpreter flag
is passed, IO works differently:

- Before execution, integer inputs from command-line arguments are pushed to the
  stack as usual.
- Above them, a new pair of functions are pushed:
  - First, a function which, when applied to an argument, reads an integer from
    STDIN in the specified format and returns it as a church numeral. `K` is
    returned instead if there is nothing left to read. EOF can be easily
    distinguished from valid input by applying it to `I`, which yields 1 for
    integers and 0 for EOF. If input is disabled with `-In`, this function is
    still pushed but always returns `K`.
  - Second, a function which, when applied to an argument, writes it to
    STDOUT/STDERR in the specified format and returns its argument unchanged. As
    with stack output, invalid Church numerals are silently ignored except in
    debug output mode. If output is disabled with `-On`, this function is still
    pushed but always silently ignores its input.
- The contents of the stack are not output at the end of execution, but the
  program return value is still output in the specified format as usual.

These functions make it possible to write programs with interactive IO.

[Flurry]: https://esolangs.org/wiki/Flurry
[Standard ML]: https://smlfamily.github.io
[MLton]: http://www.mlton.org/Home
[Standard ML of New Jersey]: https://www.smlnj.org
