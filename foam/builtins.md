# Foam Builtins

This file lists 83 builtins that are currently available in Foam, as well as what they do.

## `!)`

Pop a block and push it back without its last element.

Usage: `a: block` → `a[:-1]`

### Examples

| **Code**     | **Result** |
| ------------ | ---------- |
| `[a b c] !)` | `[a b]`    |

## `!;`

Pop an element from the stack and discard it.

Usage: `a` → 

### Examples

| **Code**            | **Result**    |
| ------------------- | ------------- |
| `' foo ' bar !; ."` | `output: foo` |

## `#`

Pop a string from the current call frame and attempt to parse it as a number.
Error if it cannot be parsed.

Usage:  → `int(call_stack.frames[0].pop(0))`

### Examples

| **Code**     | **Result** |
| ------------ | ---------- |
| `# 99 # 4 +` | `103`      |
| `# -5 # 5 +` | `0`        |

## `#.%`

Pop a number and push `1` if it is prime, `0` otherwise.
Alias for `%:# 2 =`.

Usage: `a: number` → `divisors(a).length == 2`

### Examples

| **Code**   | **Result** |
| ---------- | ---------- |
| `# 19 #.%` | `1`        |
| `# 20 #.%` | `0`        |

## `#/`

Pop a number and push a block containing its divisors.

Usage: `a: number` → `divisors(a)`

### Examples

| **Code**   | **Result**                                                             |
| ---------- | ---------------------------------------------------------------------- |
| `# 360 #/` | `[1 2 3 4 5 6 8 9 10 12 15 18 20 24 30 36 40 45 60 72 90 120 180 360]` |

## `#8`

Push 256 to the stack. Alias for `# 256`.

Usage:  → `256`

### Examples

| **Code**  | **Result** |
| --------- | ---------- |
| `#8 10 +` | `276`      |

## `%`

Modulo operation on two numbers. This uses Python's `%` operator,
where the result is only negative if the divisor is negative.

Usage: `a: number, b: number` → `a % b`

### Examples

| **Code**       | **Result** |
| -------------- | ---------- |
| `# 762 # -3 %` | `-1`       |

## `%:#`

Pop a number and push the number of divisors it has.
Alias for `/# ..#`

Usage: `a: number` → `divisors(a).length`

### Examples

| **Code** | **Result** |
| -------- | ---------- |
| `e2 %:#` | `9`        |

## `%=`

Pop two numbers and push whether the second is divisible by the first.

Usage: `a: number, b: number` → `!(a % b)`

### Examples

| **Code**    | **Result** |
| ----------- | ---------- |
| `8 4 %=`    | `1`        |
| `100 29 %=` | `0`        |

## `%?`

Randomly push either `0` or `1`. Alias for `2 ?#`.

Usage:  → `[0, 1].pick_random()`

### Examples

| **Code**                     | **Result**        |
| ---------------------------- | ----------------- |
| `[%?] : ++ : ++ : ++ : ++ ~` | `0 1 1 1 0 1 1 0` |

## `'`

Pop an element from the current call frame.

Usage:  → `call_stack.frames[1].pop(0)`

### Examples

| **Code**  | **Result** |
| --------- | ---------- |
| `' Hello` | `Hello`    |

## `(`

Remove the first element of a block and push it separately.
Leave the block on the stack.

Usage: `a: block` → `a[1:], a[0]`

### Examples

| **Code**       | **Result**   |
| -------------- | ------------ |
| `[1 4 9 16] (` | `[4 9 16] 1` |

## `(!`

Pop a block and push it back without its first element.

Usage: `a: block` → `a[1:]`

### Examples

| **Code**           | **Result**  |
| ------------------ | ----------- |
| `[foo bar baz] (!` | `[bar baz]` |

## `(!)`

Pop a block and push it back without its first or last element.

Usage: `a: block` → `[1:-1]`

### Examples

| **Code**          | **Result** |
| ----------------- | ---------- |
| `[1 2 3 4 5] (!)` | `[2 3 4]`  |

## `(*`

Pop a number `n` and push `(n - 1)/2`. Alias for `-- 2 /`.

Usage: `a: number` → `(a - 1) / 2`

### Examples

| **Code**  | **Result** |
| --------- | ---------- |
| `# 11 (*` | `5`        |
| `# 27 (*` | `13`       |

## `)`

Remove the last element of a block and push it separately.
Leave the block on the stack.

Usage: `a: block` → `a[:-1], a[-1]`

### Examples

| **Code**    | **Result** |
| ----------- | ---------- |
| `[a b c] )` | `[a b] c`  |

## `*`

Multiply two numbers.

Usage: `a: integer, b: integer` → `a * b`

### Examples

| **Code**    | **Result** |
| ----------- | ---------- |
| `# 5 # 7 *` | `35`       |

## `*)`

Pop a number `n` and push `2n + 1`. Alias for `2 * +1`.

Usage: `a: number` → `2*a + 1`

### Examples

| **Code** | **Result** |
| -------- | ---------- |
| `5 *)`   | `11`       |

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

## `+/`

Pop a block containing numbers and return the sum of all the numbers.

Usage: `a: block` → `block.sum()`

### Examples

| **Code**  | **Result** |
| --------- | ---------- |
| `5 .. +/` | `10`       |
| `[] +/`   | `0`        |

## `+1`

Increment a number. Alias for `1 +`.

Usage: `a: number` → `a + 1`

### Examples

| **Code**                                  | **Result** |
| ----------------------------------------- | ---------- |
| <pre>1 2 -1<br/># 3 {#}<br/>[+1] :%</pre> | `[2 3 0]`  |

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

## `,,`

Pop two stack elements and push a block containing them.
Alias for `2 {#}`.

Usage: `a, b` → `[a b]`

### Examples

| **Code**             | **Result**    |
| -------------------- | ------------- |
| `' foo ' bar # 3 ,,` | `foo [bar 3]` |

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

## `-`

Subtract two numbers.

Usage: `a: number, b: number` → `a - b`

### Examples

| **Code** | **Result** |
| -------- | ---------- |
| `1 -1 -` | `2`        |

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

## `--`

Decrement a number. Alias for `1 -`.

Usage: `a: number` → `a - 1`

### Examples

| **Code** | **Result** |
| -------- | ---------- |
| `-1 --`  | `-2`       |

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

| **Code**                                  | **Result** |
| ----------------------------------------- | ---------- |
| <pre>1 -\> a<br/>10 -\> b<br/>b a -</pre> | `9`        |

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

## `.-`

Pop a number and push a reversed range. Alias for `.. -%`.

Usage: `a: number` → `[a-1..0]`

### Examples

| **Code** | **Result**    |
| -------- | ------------- |
| `# 5 .-` | `[4 3 2 1 0]` |

## `..`

Pop a number and push a block containing all nonnegative numbers less than it,
in sorted order.

Usage: `a: number` → `[0..a-1]`

### Examples

| **Code**              | **Result**                         |
| --------------------- | ---------------------------------- |
| `[0 2 10] [># ..] :%` | `[[] [0 1] [0 1 2 3 4 5 6 7 8 9]]` |

## `..#`

Pop a block and push its length.

Usage: `a: block` → `a.length`

### Examples

| **Code**    | **Result** |
| ----------- | ---------- |
| `10 .. ..#` | `10`       |

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

## `/`

Integer division on two numbers. This uses Python's `//` operator,
which always rounds towards negative infinity.

Usage: `a: number, b: number` → `a/b`

### Examples

| **Code**       | **Result** |
| -------------- | ---------- |
| `# 123 # 11 /` | `11`       |

## `/%`

Pop two numbers and push their divmod.
Alias for `[/] [%] 2 ~#~`.

Usage: `a: number, b: number` → `a/b, a%b`

### Examples

| **Code**        | **Result** |
| --------------- | ---------- |
| `# 105 # 20 /%` | `[5 5]`    |

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

## `2%`

Modulo by 2; push `0` if the input is even and `1` if it is odd.

Usage: `a: number` → `a % 2`

### Examples

| **Code** | **Result** |
| -------- | ---------- |
| `-196`   | `0`        |
| `3`      | `1`        |

## `:`

Pop an element from the stack and push back twice.

Usage: `a` → `a, a`

### Examples

| **Code** | **Result** |
| -------- | ---------- |
| `e2 : +` | `200`      |

## `:#`

Pop a block and push it back, along with its length.

Usage: `a: block` → `a, a.length`

### Examples

| **Code**           | **Result**        |
| ------------------ | ----------------- |
| `[a b c d e f] :#` | `[a b c d e f] 6` |

## `:%`

Pop two blocks. Treat the second as a function and map it over the first.

Usage: `a: block, b: block` → `map(b, a)`

### Examples

| **Code**             | **Result** |
| -------------------- | ---------- |
| `[1 2 3] [># 1-] :%` | `[0 1 2]`  |

## `:/`

Pop two blocks. Treat the second as a function and map it over the first.
Rather than collecting the results into a new block, dump them onto the stack.

Usage: `a: block, b: block` → `...map(b, a)`

### Examples

| **Code**             | **Result** |
| -------------------- | ---------- |
| `[1 2 3] [># 1-] :/` | `0, 1, 2`  |

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

## `<->`

Palindromize: append a block and its reverse with the first element dropped.

Usage: `a: block` → `a ++ reverse(a[1:])`

### Examples

| **Code**      | **Result**    |
| ------------- | ------------- |
| `[a b c] <->` | `[a b c b a]` |

## `=`

Pop two items and push `1` if they are equal, and `0` otherwise.
Elements of different types are never considered equal.

Usage: `a, b` → `!(a == b)`

### Examples

| **Code**                                            | **Result** |
| --------------------------------------------------- | ---------- |
| `2 : + # 4 =`                                       | `1`        |
| `# 5 ' 5 =`                                         | `0`        |
| <pre>[0 1 2] [\>#] :%<br/>0 1 2 # 3 {#}<br/>=</pre> | `1`        |

## `=#`

Pop an index and a block, then return the element of the block
at that index. Indexing is 0-based and cyclic, so the 4th element
of `[a b c]` is considered to be `b`.

Usage: `a: block, b: number` → `a[b % a.length]`

### Examples

| **Code**         | **Result** |
| ---------------- | ---------- |
| `[a b c] # 4 =#` | `b`        |
| `# 5 .- 1 =#`    | `3`        |

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

## `>-<`

Reverse palindromize. Alias for `-% <->`.

Usage: `a: block` → `reverse(a) ++ a[1:]`

### Examples

| **Code**            | **Result**              |
| ------------------- | ----------------------- |
| `[foo bar baz] >-<` | `[baz bar foo bar baz]` |

## `?#`

Pop a number and return a uniformly random nonnegative integer
that is less than it.

Usage: `a: number` → `floor(random() * a)`

### Examples

| **Code**  | **Result** |
| --------- | ---------- |
| `# e3 ?#` | `413`      |

## `?=`

Pop a block and return a randomly chosen element of that block.

Usage: `a: block` → `a.pick_random()`

### Examples

| **Code**                | **Result** |
| ----------------------- | ---------- |
| `[foo bar baz quux] ?=` | `baz`      |

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

Usage: `a: block` → `...a`

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

## `| |`

Push a space to the stack.

This is a literal space token, and can be written as `| |` in the source code.

Usage:  → `" "`

### Examples

| **Code**       | **Result**  |
| -------------- | ----------- |
| `| | {_} : ++` | `[| | | |]` |

## `|\n|`

Push a newline to the stack.

This is a literal newline token, and can be written as

    |
    |

in the source code.

Usage:  → `"\n"`

### Examples

| **Code**                       | **Result**                       |
| ------------------------------ | -------------------------------- |
| <pre>\|<br/>\| {\_} : ++</pre> | <pre>[\|<br/>\| \|<br/>\|]</pre> |

## `~`

Execute  the given block in a new call frame.

Usage: `a: block` → `call_stack.add_frame(a)`

### Examples

| **Code**    | **Result** |
| ----------- | ---------- |
| `1 2 [+] ~` | `3`        |

## `~#~`

Pop a number `n` and two blocks.
Execute both blocks, sharing `n` stack elements.

Usage: `...s, a: block, b: block, n: number` → `b(a(s) + s[-n:])`

### Examples

| **Code**            | **Result** |
| ------------------- | ---------- |
| `5 [1+] [--] 1 ~#~` | `4, 6`     |
| `3 5 [+] [-] 2 ~#~` | `8, -2`    |

## `~*`

Execute the given block in the current call frame.

Usage: `a: block` → `call_stack.frames[0].extend_front(a)`

### Examples

| **Code**       | **Result** |
| -------------- | ---------- |
| `1 2 ['] ~* +` | `1, 2, +`  |