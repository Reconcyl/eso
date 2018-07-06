# Builtins

## `'`

Pops an element from the current call frame.

Usage:  → `call_stack.frames[1].pop(0)`

### Examples

| **Code**  | **Result** |
| --------- | ---------- |
| `' Hello` | `Hello`    |

## `+`

Adds two numbers.

Usage: `a: integer, b: integer` → `a + b`

### Examples

| **Code** | **Result** |
| -------- | ---------- |
| `1 2 +`  | `3`        |

## `++`

Concatenates two blocks.

Usage: `a: block, b: block` → `a ++ b`

### Examples

| **Code**         | **Result**  |
| ---------------- | ----------- |
| `[a b] [c d] ++` | `[a b c d]` |

## `,@`

Pops an element from the caller's call frame.

Usage:  → `call_stack.frames[1].pop(0)`

### Examples

| **Code**       | **Result** |
| -------------- | ---------- |
| `1 2 [,@] ~ +` | `1, 2, +`  |

## `.`

Pops a string and outputs the uneval'ed form.

Usage: `a: string` → `output(uneval(a))`

### Examples

| **Code**         | **Result**           |
| ---------------- | -------------------- |
| `' |Hi there| .` | `output: |Hi there|` |

## `."`

Pops a string and outputs it.

Usage: `a: string` → `output(a)`

### Examples

| **Code**               | **Result**              |
| ---------------------- | ----------------------- |
| `' |Hello, World!| ."` | `output: Hello, World!` |

## `.#`

Pops a number and outputs its decimal representation.

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

## `//`

Swaps the top two elements on the stack.

Usage: `a, b` → `b, a`

### Examples

| **Code**       | **Result** |
| -------------- | ---------- |
| `1 ' Hello //` | `Hello, 1` |

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

## `@`

Reverses the order of the first 3 stack items.

Usage: `a, b, c` → `c, b, a`

### Examples

| **Code**              | **Result**          |
| --------------------- | ------------------- |
| `1 ' Hello [a b c] @` | `[a b c], Hello, 1` |

## `_}`

Appends an element to a list.

Usage: `a: block, b` → `a ++ [b]`

### Examples

| **Code**          | **Result**     |
| ----------------- | -------------- |
| `[1 2 3 4] 10 _}` | `[1 2 3 4 10]` |

## `{#}`

Pops `N` and turns the first `N` stack items into a block.

Usage: `...s, a: integer` → `...s[:a], s[a:]`

### Examples

| **Code**                     | **Result**     |
| ---------------------------- | -------------- |
| `' a ' b ' c ' d 'e # 4 {#}` | `a, [b c d e]` |

## `{_`

Prepends an element to a list.

Usage: `a, b: block` → `[a] ++ b`

### Examples

| **Code**       | **Result**  |
| -------------- | ----------- |
| `1 [2 3 4] {_` | `[1 2 3 4]` |

## `{_'`

Like `{_`, but takes arguments in the opposite order.

Usage: `a: block, b` → `[b] ++ a`

### Examples

| **Code**        | **Result**  |
| --------------- | ----------- |
| `[2 3 4] 1 {_'` | `[1 2 3 4]` |

## `{_}`

Wraps its argument in a block.

Usage: `any` → `[a]`

### Examples

| **Code**      | **Result** |
| ------------- | ---------- |
| `' Hello {_}` | `[Hello]`  |

## `~`

Executes the given block in a new call frame.

Usage: `a: block` → `call_stack.add_frame(a)`

### Examples

| **Code**    | **Result** |
| ----------- | ---------- |
| `1 2 [+] ~` | `3`        |

## `~*`

Executes the given block in the current call frame.

Usage: `a: block` → `call_stack.frames[0].extend_front(a)`

### Examples

| **Code**       | **Result** |
| -------------- | ---------- |
| `1 2 ['] ~* +` | `1, 2, +`  |