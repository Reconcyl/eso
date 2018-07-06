# Foam-Lang

Foam stands for *Functional Obfuscated ASCII Mess*. Inspired by Forth,
it aims to be the world's most unreadable programming language.

Here at Foam, we strongly believe that the emphasis towards readability
in modern-day languages was a step in the wrong direction. Foam aims
to correct this error.

## Using the interpreter

The interpreter is written in Python 3. You can verify the tests using

    $ cd test 
    $ python3 test.py

You can run the interpreter using

    $ python3 foam.py <file>

The interpreter also supports using `-i` to specify an input file:

    $ python3 foam.py -i inputs.txt code.foam

You can also use `-t` to enable tail-call optimization. However, this
may break self-modifying programs.

A list of builtins is available in `builtins.md`.

## Contributing

I plan to write a basic language description soon. If anyone is able
to understand the source code and would like to add more builtins,
this is the process to do it:

- Add it to `builtin.py`. If the builtin can be bootstrapped in Foam,
  use `alias(name, code)` to declare it. If the builtin needs to be
  written in Python, use the `@core(name)` decorator.
- Add documentation for it by updating `builtins_raw.txt` with the
  builtin's name, description, usage string, and an example or two.
- Run `gen_doc.py`, which parses `builtins_raw.txt` and generates
  Markdown in `builtins.md`