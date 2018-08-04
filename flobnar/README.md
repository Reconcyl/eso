# Flobnar

**For all your drunk Befunge programming needs!**

This is a Rust implementation of [the Flobnar esoteric programming language](https://github.com/catseye/Flobnar). You can follow that link to find a language specification and the reference interpreter, written in Haskell.

This implementation supports all behavior mentioned in the spec and runs all of the example programs correctly (including randomness, i/o, etc.). It expects programs to be encoded in UTF-8, and will probably panic if a non-UTF-8 file is supplied.

## Using the interpreter

To use the interpreter, download the project and build it with `cargo build --release`. This will generate an executable called `flobnar` in the subdirectory `target/release`.

The interpreter should be passed an argument representing the name of the program (the spec did not mention a canonical file extension, nor could I find any evidence that one existed, so I use `.flob`). It also supports the following arguments:

- `--wrap-on-invalid-output`: The Flobnar spec leaves outputting a number outside the range of ASCII undefined. Without this flag, the interpreter will always output a number if it can fit in a byte, and will exit with an error message otherwise. With this flag, the number will be truncated to fit in a byte before being output.
- `--ignore-invalid-terms`: The Flobnar spec also leaves executing terms that aren't valid commands undefined. Without this flag, the interpreter will exit with an error message if a program tries to execute one. With this flag, they will all be treated the same as space (in terms of executed behavior, not wrapping behavior).
- `--enable-decimal-io-extension`: Using this flag makes `.` and `&` valid commands for "output as decimal" and "input as decimal" respectively. Output is done without any separator; input will ignore all bytes until it finds a digit or `-`, and will ignore a single byte afterwards. Yes; I know; that's weird. Deal with it.
- `--suppress-final-result`: The spec leaves the destination of the result of the program unspecified. By default, this interpreter will send it to STDOUT. With this flag enabled, it will ignore it.

There are a few other areas where I took liberties because the spec wasn't entirely clear:

- `~` and `&` evaluate and return the term to their south if they encounter EOF.
- Cell coordinates (those used by `g` and `p`) are "absolute": the first character in the original program text is considered to lie at `(0, 0)`, and changes in the bounding box don't affect this. You can use `p` to extend the bounding box to include coordinates in the negatives, and this will change the bounding box, which affects how `#` works, but it won't change the "origin" of the coordinate system. I have no clue whether this is what the reference interpreter does, and I'm too lazy to figure it out.

> Absolutely no-one cares about Flobnar.

— Chris Pressey in [Befunge Silver Jubilee Retrospective](http://catseye.tc/view/The-Dossier/article/Befunge%20Silver%20Jubilee%20Retrospective.md)

Not anymore!