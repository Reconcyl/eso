An interpreter for [brainfuck](https://en.wikipedia.org/wiki/Brainfuck), written in [whitespace](https://en.wikipedia.org/wiki/Whitespace_(programming_language)).

More specifically, the interpreter is written in an ASM-like language that compiles 1-to-1 into a Whitespace program. You can find the original source code in `bf.wsasm`. Compilation is handled by [`whitespace-rs`](https://github.com/CensoredUsername/whitespace-rs), so if you have it installed, you can compile it as follows:

    $ wsc -t -m -f asm bf.wsasm >bf.ws

The resulting program can be executed using the same tool:

    $ wsc --unchecked-heap bf.ws

The program is read from STDIN until a `!` character is encountered, after which all bytes will be considered input to the program.

    $ echo '+++[->,+.<],.!foo' | wsc --unchecked-heap bf.ws
    gpp

The version of BF simulated by this interpreter has an unbounded number of 8-bit cells to the right. Moving left of the starting cell doesn't work.
