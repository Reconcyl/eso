# Imma

**TL;DR:** Imma is a programming language where instructions only support immediate addressing. For example, consider the following statement:

```
add 3, 4
```

As expected, it computes `3 + 4`, but it does not store the result to a stack, registers, or any other external source. Instead, the instruction is stored back into the argument, causing the statement to become:

```
add 7, 4
```

How can this result be used by other instructions? The answer is that Imma programs operate in a virtual von Neumann machine, so the program contents are accessible in memory. Each opcode (the instruction name, like `add`) occupies one cell, and each of its operands does as well. Thus, if the `add` instruction were located at position 23, the result could be fetched using the `get` instruction:

```
get 24
```

After being executed, this would become:

```
get 7
```

Of course, this result is not accessible either. We could keep copying it around forever, but in order to actually use it, we must instead *write* the code that uses it to the location. For this, we use the `lit` command:

```
lit num, 23 ; replace the `add` instruction with `num`
lit nop, 25 ; replace the `4` operand with a dummy instruction
```

Now the program has become:

```
num 7, nop
```

The `num` instruction takes its operand and outputs it directly. `nop` doesn't do anything; it's just there to ensure the interpreter doesn't try to execute an operand as the opcode for the next instruction.

Great, now we've done something useful with the result! Unfortunately, we've also completely clobbered the original `add` instruction, so we'll have to use more `lit` commands to write it back.

## Execution

Imma programs execute in an array of cells. Each cell is an unsigned 16-bit integer. There are as many cells as there are unsigned 16-bit integers.

The first cell (i.e. the one with address `0`) stores the instruction pointer. On every cycle, the instruction pointer is dereferenced to get an opcode. Depending on this opcode, the next few cells may also be used as operands. In other words, if you remember [Dreaderef](https://github.com/Reconcyl/dreaderef) — it's a lot like that, just with a limited address space.

There are thirteen instructions:

| Opcode | Name | Args | Effect
| - | - | - | -
| `0` | `hlt` | None | Stop execution.
| `1` | `nop` | None | Do nothing.
| `2` | `get` | `a` | `a = [a]`
| `3` | `lit` | `a b` | `[b] = a`
| `4` | `not` | `a` | `a = !a`
| `5` | `add` | `a b` | `a = a + b`
| `6` | `mul` | `a b` | `a = a * b`
| `7` | `max` | `a b` | `a = max(a, b)`
| `8` | `dmp` | `a b c` | Read `c` cells from external memory (extension)
| `9` | `sav` | `a b c` | Write `c` cells to external memory (extension)
| `10` | `chr` | `a` | Output the low byte of `a` as a character
| `11` | `num` | `a` | Output `a` as a number with no separator
| `12` | `chi` | `a` | Assign `a` to an input character, or `-1` on EOF

All other opcodes are equivalent to `nop`.

### How do I access more than 65K cells?

You can't. Be grateful, it's more than brainfuck gives you.

### I really want to though

There's an extension for it. In implementations that support the extension, there's another array of 2^32 cells, each 16 bits. These cells are addressed using little-endian pairs of 16 bit integers (so `(65535, 0)` and `(0, 1)` are consecutive addresses).

Reading from this memory array is done using the `dmp` instruction. It takes three arguments and reads `c` cells from external memory starting at location `(a, b)`. These cells are placed into memory immediately following the `dmp` command, overwriting whatever was already there (of course, this means they will start being executed as instructions, so I hope you prepared for that).

Writing to the memory array is done using the `sav` instruction. It takes three arguments and writes the next `c` cells to external memory starting at location `(a, b)`.

In theory, a conforming implementation must provide the illusion that the entire external memory array is available from program startup and its contents are zero-initialized. In practice, a conforming implementation is discouraged from immediately allocating 8 GB of memory.

## Syntax

The native representation of an Imma program is a 131,072-byte array, interpreted as 65,536 little-endian 16-bit integers. A smaller array can also be used, with the understanding that all addresses higher than those specified in the program will contain `0`.

When such a representation is stored in a file, it should use the extension `.immi` (short for "Imma image", and a reference to the fact that preprocessed C code uses the extension `.i`).

There is also a preprocessor which aims to provide a more convenient syntax. Code written with the preprocessor should be stored with the extension `.imma`.

```
; comments are prefixed by semicolons
1           ; numbers can be embedded directly
nop         ; opcodes indicate numbers
lit foo 0   ; labels can also be used
foo: nop    ; labels are defined using a trailing colon
chr "\n\nA" ; string literals expand to their byte values, so this is 10 10 10 65
get foo-1   ; labels can have offsets using `+` and `-`
get $+1     ; a special label which refers to the current location
?           ; an unspecified value (conveys that it will be assigned at runtime)
```

The file `syntax.vim` provides Vim syntax highlighting for the preprocessor syntax.

## Implementation

The preprocessor is written in [Zig](https://ziglang.org) and should be reasonably portable. It can be compiled using:

```
zig build-exe --single-threaded -O ReleaseSafe ppx.zig
./ppx input.imma output.immi
```

There are also two interpreters, one written in portable C, and the other written in x86 assembly. If you already have Zig installed, you can compile either of them using `zig cc`:

```
zig cc -Wall -Wextra -Wpedantic -O3      exec.c -o exec # C
zig cc -Wno-unused-command-line-argument exec.s -o exec # assembly
./exec input.immi
./exec -q input.immi # suppress warnings if the file is smaller than 131K
```

The assembly interpreter should be considered the reference, but it's Linux-specific and doesn't support the external memory extension. The C interpreter has a small optimization (caching the IP in a register) which might result in bugs.

There is a Makefile, but all it knows how to do is compile those two programs.
