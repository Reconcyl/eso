# Aubers

This is a Rust implementation of the [Aubergine](https://esolangs.org/wiki/Aubergine) programming language.

## About

This implementation represents the program/data tape as an array of `BigInt`s. Attempting to output a number that does not fit in a byte will produce an error. Reading input returns `-1` on EOF.

Internally, a large fraction of instruction types are implemented using dedicated logic. This prevents us from having to allocate and copy unnecessary `BigInt`s for cases like `+AA`, but it also means that there are likely some bugs.

## Usage

Build:

    $ cargo build --release

Run a program from a file:

    $ printf '=aA-a1=oA=bi+b1-Ab-bb:bA+B1=iBGolf by Quintopia\n!dlroW ,olleH' > test.aub
    $ ./aubers test.aub
    Hello, World!

Run a program from an argument:

    $ ./aubers -c '=ii=oo=ib'
    foo
    foo
    bar
    bar

