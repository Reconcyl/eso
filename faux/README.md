# faux

This project contains a number of files, the end result of which is a program written in [FALSE](https://esolangs.org/wiki/FALSE), that is itself capable of compiling FALSE programs to x86-64 assembly ([NASM](https://www.nasm.us/) dialect).

The compiler, published here as `faux.f`, takes the input program on STDIN and writes the resulting assembly to STDOUT. The result of passing the compiler as input to itself (bootstrapping) is published here as `faux.asm`.

Note, however, that neither of these files were written directly. Instead, `faux.f` was generated by `Main.hs`, which provides:

- An abstract syntax tree;
- A set of functions to create ASTs in a more verbose, but clearer, notation;
- An implementation of the compiler in terms of these functions;
- A pretty-printer to convert ASTs into FALSE programs;
- And an interpreter which executes ASTs directly.

It takes the compiler's AST and converts it to a FALSE program - that's `faux.f`. Then it uses the AST interpreter to run the compiler with the FALSE program as input - that's `faux.asm`.

The compiler itself is very minimal, doing essentially just one pass (or maybe 1.5) with no optimization done on generated code, except one that happens by accident. You'll see a lot of unnecessary stack manipulation, like `push rax` / `pop rax`. However, because FALSE is such a simple language, and we don't bother to check for things like stack underflow, most instructions can be implemented in a relatively short and fast assembly sequence.

In implementing FALSE on x86-64, I have tried to stay true to the original implementation where possible, but some changes were necessary:

- All instructions described in the original release of the language are supported.
- Stack items are 64 bits rather than 16.
- Variable references are implemented as pointers, so `;` and `:` do arbitrary fetches and stores.
- Functions are implemented as function pointers.
- The stack has a limit of 2^20 elements (8 MB) and grows downward so we can use the ordinary stack manipulation instructions.
- Division and comparisons treat stack elements as signed, but `.` outputs elements as unsigned integers.
- `a` initially contains a pointer to command-line arguments (although it is hard to actually read these, since there is no way to load individual bytes).
- `` ` `` has the same literal functionality in that it embeds a pair of bytes into the code directly (big-endian). However, these bytes will obviously be treated as x86 instructions rather than 68k instructions.
- I make no guarantees about the behavior of the compiler when the input program is syntactically invalid. You could probably hijack it with a buffer overflow somehow.
- In the spirit of being platform-specific, the generated code will only work on MacOS. It should be portable to Linux, however; the only differences should be syscall numbers (which can be easily changed in `Main.hs`) and the executable format passed to NASM (which can be changed in `make`).
