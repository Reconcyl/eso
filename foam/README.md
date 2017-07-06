# Foam-Lang

Foam stands for *Functional Obfuscated ASCII Mess*. Inspired by Forth,
it aims to be the world's most unreadable programming language.

Here at Foam, we strongly believe that the emphasis towards readability
in modern-day languages was a step in the wrong direction. Foam aims
to correct this error.

## Using the interpreter

The interpreter is written in Python 3. You can verify the tests using

    $ python3 test/test.py

You can run the interpreter using

    $ python3 foam.py <file>

The interpreter also supports using `-i` to specify an input file:

    $ python3 foam.py -i inputs.txt code.foam

You can also use `-t` to enable tail-call optimization. However, this
may break self-modifying programs.

You can find more information about the language on the wiki.