# check-lang

Repository for the Check esoteric programming language.

## Using the interpreter

Run the interpreter as:

    path/to/python3 path/to/src/check.py path/to/code.chk [initial stack values]

## The Language

Check is a combination of a 1-D and 2-D language. Stack manipulation is done in 1-D, but control flow is done in 2-D.

Check operates on a stack. There are two data types that can be manipulated:

- Unsigned Integer
- Array

With two-dimensional semantics, control can be going in any of four directions: up, right, down, and left. The instructions `^>v<` redirect the instruction pointer in their respective directions, and `#` switches back to one-dimensional mode.

With one-dimensional semantics, control always proceeds forwards. When it reaches the end of the line, it proceeds to the next. It has the following instructions:

- `>` pushes 0 to the stack.
- `0-9` multiply the number on the stack by 10 and then add the value of their respective digits.
- `+` adds two numbers or concatenates two arrays.
- `-` subtracts two numbers.
- `_` negates a number or reverses an array.
- `*` multiplies two numbers or repeats an array a certain number of times.
- `$` divides a number by 2.
- `%` preforms modulo on two numbers.
- `o` On an integer, it outputs its character code. On an array, it flattens it and outputs it as a list of character codes. Output is without a trailing newline.
- `)` increments an integer.
- `(` decrements an integer.
- `p` prints an integer or array, the way Python 3 would. Output is without a trailing newline. It does not pop the value on the stack.
- `<` outputs a newline (equivalent to `>10o`).
- `!` preforms boolean NOT. If the integer is `0` or the array is empty, it pushes `1`. Otherwise, it pushes `0`.
- `[` pushes an empty array.
- `]` wraps an element in an array.
- `#` switches to 2-D mode, initially pointing to the left.
- `?` switches to 2-D mode if the top stack value is 0. It does not delete the stack value.
- `"` starts a string literal. The code points of each character in the string literal are collected in an array. `\` is used as an escape character.
- `:` duplicates the top stack element.
- `\` swaps the top two stack elements.
- `@` rotates the top three stack elements.
- `;` pops `n` and rotates the top `n` stack items. `\` and `@` can be seen as special cases with `>2;` and `>3;` respectively.
- `'` is like `;`, but it rotates in the other direction.
- `d` deletes the top stack item.
- `.` makes the entire stack into an array.
- `&` unpacks all array values onto the stack.
- `,` gets a range `[0,n)` on an integer and gets the length of an array.
- `{` gets the element of an array at an integer index.
- `r` pops a value and stores it in the register.
- `R` pushes the value from the register onto the stack.
- `` ` `` prints everything in the stack for debugging.

Attempting to execute an invalid instruction in either language will error. If the interpreter ever prints a stack trace, there was a bug.
