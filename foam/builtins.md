# Foam Builtins

This file lists 45 builtins that are currently available in Foam, as well as what they do.

## `#`

Pop a string from the current call frame and attempt to parse it as a number.
Error if it cannot be parsed.

Usage:  → `int(call_stack.frames[0].pop(0))`

### Examples

| **Code**     | **Result** |
| ------------ | ---------- |
| `# 99 # 4 +` | `103`      |
| `# -5 # 5 +` | `0`        |

## `#8`

Push 256 to the stack. Alias for `# 256`.

Usage:  → `256`

### Examples

| **Code**  | **Result** |
| --------- | ---------- |
| `#8 10 +` | `276`      |

## `'`

Pop an element from the current call frame.

Usage:  → `call_stack.frames[1].pop(0)`

### Examples

| **Code**  | **Result** |
| --------- | ---------- |
| `' Hello` | `Hello`    |

## `+`

Add two numbers.

Usage: `a: integer, b: integer` → `a + b`

### Examples

| **Code** | **Result** |
| -------- | ---------- |
| `1 2 +`  | `3`        |

## `++`

Concatenate two blocks.

Usage: `a: block, b: block` → `a ++ b`

### Examples

| **Code**         | **Result**  |
| ---------------- | ----------- |
| `[a b] [c d] ++` | `[a b c d]` |

## `,`

Read a character from STDIN and push it as a string.

Usage:  → `stdin.get_char()`

### Examples

| **Code** | **Result**                         |
| -------- | ---------------------------------- |
| `, ."`   | `output: first character of input` |

## `,*`

Read the entirety of input from STDIN and push it as a string.

Usage:  → `stdin.read()`

### Examples

| **Code** | **Result**    |
| -------- | ------------- |
| `,* ."`  | `cat program` |

## `,@`

Pop an element from the caller's call frame.

Usage:  → `call_stack.frames[1].pop(0)`

### Examples

| **Code**       | **Result** |
| -------------- | ---------- |
| `1 2 [,@] ~ +` | `1, 2, +`  |

## `,@*`

Push the entirety of the caller's call frame.

Usage:  → `call_stack.frames[1].copy()`

### Examples

| **Code**             | **Result**   |
| -------------------- | ------------ |
| `[,@* ~] ~ 2 : + .#` | `output: 44` |

## `,@*'`

Push the entirety of the current call frame.

Usage:  → `call_stack.frames[0].copy()`

### Examples

| **Code**      | **Result**                |
| ------------- | ------------------------- |
| `,@*' : ++ .` | `output: [: ++ . : ++ .]` |

## `-%`

Pop a block and push it reversed.

Usage: `a: block` → `reverse(a)`

### Examples

| **Code**     | **Result** |
| ------------ | ---------- |
| `[a b c] -%` | `[c b a]`  |

## `-*`

Pop a number and push its additive inverse.

Usage: `a: number` → `-n`

### Examples

| **Code** | **Result** |
| -------- | ---------- |
| `10 -*`  | `-10`      |
| `-1 -*`  | `1`        |

## `-1`

Push -1 to the stack. Alias for `# -1`.

Usage:  → `-1`

### Examples

| **Code** | **Result** |
| -------- | ---------- |
| `1 -1 +` | `0`        |

## `->`

Pop an element from the stack and a name from the current call
frame, then redefine the operation with that name to push the
element.

Usage: `a` → `commands[call_stack.frames[0].pop()] = blockify(a)`

### Examples

| **Code**                 | **Result** |
| ------------------------ | ---------- |
| `1 -> a  10 -> b  b a -` | `9`        |

## `.`

Pop a list or string and output the uneval'ed form.

Usage: `a: string` → `output(uneval(a))`

### Examples

| **Code**            | **Result**                |
| ------------------- | ------------------------- |
| `' |Hi there| .`    | `output: |Hi there|`      |
| `[|foo bar| baz] .` | `output: [|foo bar| baz]` |

## `."`

Pop a string and output it.

Usage: `a: string` → `output(a)`

### Examples

| **Code**               | **Result**              |
| ---------------------- | ----------------------- |
| `' |Hello, World!| ."` | `output: Hello, World!` |

## `.#`

Pop a number and output its decimal representation.

Usage: `a: number` → `output(a)`

### Examples

| **Code**     | **Result**   |
| ------------ | ------------ |
| `10 10 + .#` | `output: 20` |

## `.'`

Pop a string from the current call frame and output it.

Usage:  → `output(call_stack.frames[0].pop(0))`

### Examples

| **Code** | **Result**    |
| -------- | ------------- |
| `.' foo` | `output: foo` |

## `.@`

Pop an element from the stack and add it to the caller's frame.

Usage: `a` → `call_stack.frames[1].prepend(a)`

### Examples

| **Code**                 | **Result**        |
| ------------------------ | ----------------- |
| `10 ' foo [' < .@] ~ .#` | `output: foo\n10` |

## `.@*`

Pop a block from the stack and dump its contents into the caller's frame.

Usage: `a: block` → `call_stack.frames[1].extend_front(a)`

### Examples

| **Code**              | **Result**   |
| --------------------- | ------------ |
| `10 [[1 +] .@*] ~ .#` | `output: 11` |

## `//`

Swap the top two elements on the stack.

Usage: `a, b` → `b, a`

### Examples

| **Code**       | **Result** |
| -------------- | ---------- |
| `1 ' Hello //` | `Hello, 1` |

## `0`

Push 0 to the stack. Alias for `# 0`.

Usage:  → `0`

### Examples

| **Code**  | **Result** |
| --------- | ---------- |
| `0 # 3 +` | `2`        |

## `1`

Push 1 to the stack. Alias for `# 1`.

Usage:  → `1`

### Examples

| **Code** | **Result** |
| -------- | ---------- |
| `1 10 +` | `11`       |

## `10`

Push 10 to the stack. Alias for `# 10`.

Usage:  → `10`

### Examples

| **Code**  | **Result** |
| --------- | ---------- |
| `10 10 +` | `20`       |

## `2`

Push 2 to the stack. Alias for `# 2`.

Usage:  → `2`

### Examples

| **Code**    | **Result** |
| ----------- | ---------- |
| `1 2 + 2 +` | `5`        |

## `:`

Pop an element from the stack and push back twice.

Usage: `a` → `a, a`

### Examples

| **Code** | **Result** |
| -------- | ---------- |
| `e2 : +` | `200`      |

## `;#`

Read an integer from STDIN.

Usage:  → `stdin.get_integer()`

### Examples

| **Code** | **Result**          |
| -------- | ------------------- |
| `;# : +` | `output: input * 2` |

## `<`

Output a string with a trailing newline.

Usage: `a: string` → `output(a + '\n')`

### Examples

| **Code** | **Result**    |
| -------- | ------------- |
| `' a <`  | `output: a\n` |

## `<'`

Pop a string from the current call frame and output it,
with a trailing newline.

Usage:  → `output(call_stack.frames[0].pop(0) + '\n')`

### Examples

| **Code**    | **Result**         |
| ----------- | ------------------ |
| `.' Hello!` | `output: Hello!\n` |

## `=.`

Pop a string and a block. Redefine the command with the string as its name
to execute the block.

Usage: `a: block, b: string` → `commands[string] = block`

### Examples

| **Code**            | **Result** |
| ------------------- | ---------- |
| `[+] ' - =. 10 2 -` | `12`       |

## `>"`

Pop an integer and push its string representation.

Usage: `a: number` → `str(a)`

### Examples

| **Code**            | **Result** |
| ------------------- | ---------- |
| `# 3 # 4 >" . >" .` | `43`       |

## `>#`

Pop a string and attempt to parse it as a number.
Error if it cannot be parsed.

Usage: `a: string` → `int(a)`

### Examples

| **Code** | **Result** |
| -------- | ---------- |
| `' 5 >#` | `5`        |

## `@`

Reverse the order of the first 3 stack items.

Usage: `a, b, c` → `c, b, a`

### Examples

| **Code**              | **Result**          |
| --------------------- | ------------------- |
| `1 ' Hello [a b c] @` | `[a b c], Hello, 1` |

## `_}`

Append an element to a list.

Usage: `a: block, b` → `a ++ [b]`

### Examples

| **Code**          | **Result**     |
| ----------------- | -------------- |
| `[1 2 3 4] 10 _}` | `[1 2 3 4 10]` |

## `_~`

Pop a block from the stack and push its elements back, one by one.

Usage: `a: block` → `...acal`

### Examples

| **Code**                   | **Result** |
| -------------------------- | ---------- |
| `10 [1 2] _~ ># // ># + +` | `13`       |

## `` ` ``

Pop an element and generate a string representation.

Usage: `a: string` → `uneval(a)`

### Examples

| **Code**         | **Result**      |
| ---------------- | --------------- |
| ``[a |b c|] ` `` | `|[a \|b c\|]|` |

## `` `*``

Pop an element and push a block that, when evaluated,
will push that element.

Usage: `a` → `blockify(a)`

### Examples

| **Code**      | **Result** |
| ------------- | ---------- |
| ``# 3 >" `*`` | `[' 3]`    |

## `e2`

Push 100 to the stack. Alias for `# 100`.

Usage:  → `100`

### Examples

| **Code** | **Result** |
| -------- | ---------- |
| `e2 : +` | `200`      |

## `e3`

Push 1000 to the stack. Alias for `# 1000`.

Usage:  → `1000`

### Examples

| **Code**  | **Result** |
| --------- | ---------- |
| `e3 e2 -` | `900`      |

## `{#}`

Pop `N` and turn the first `N` stack items into a block.

Usage: `...s, a: integer` → `...s[:a], s[a:]`

### Examples

| **Code**                     | **Result**     |
| ---------------------------- | -------------- |
| `' a ' b ' c ' d 'e # 4 {#}` | `a, [b c d e]` |

## `{_`

Prepend an element to a list.

Usage: `a, b: block` → `[a] ++ b`

### Examples

| **Code**       | **Result**  |
| -------------- | ----------- |
| `1 [2 3 4] {_` | `[1 2 3 4]` |

## `{_'`

Like `{_`, but take arguments in the opposite order.

Usage: `a: block, b` → `[b] ++ a`

### Examples

| **Code**        | **Result**  |
| --------------- | ----------- |
| `[2 3 4] 1 {_'` | `[1 2 3 4]` |

## `{_}`

Pop an element and wrap it in a singleton block.

Usage: `any` → `[a]`

### Examples

| **Code**      | **Result** |
| ------------- | ---------- |
| `' Hello {_}` | `[Hello]`  |

## `~`

Execute  the given block in a new call frame.

Usage: `a: block` → `call_stack.add_frame(a)`

### Examples

| **Code**    | **Result** |
| ----------- | ---------- |
| `1 2 [+] ~` | `3`        |

## `~*`

Execute the given block in the current call frame.

Usage: `a: block` → `call_stack.frames[0].extend_front(a)`

### Examples

| **Code**       | **Result** |
| -------------- | ---------- |
| `1 2 ['] ~* +` | `1, 2, +`  |