# betaload-lang

Betaload is a derivative of Underload. It is backwards compatible with Underload, and it was designed to be more useful.

One of the biggest changes to Betaload is the concept of *environments*. An environment is a space where you can execute commands. In Underload, there is only one environment, which is the environment containing the main stack. In Betaload, this is just the *root environment*. Every environment has two stacks: a stack of strings, and a stack of sub-environments. Environments can be dynamically created and data can be sent to and from them.

Each Betaload environment can be in one of two modes: S-mode or E-mode. The environment's mode determines whether certain commands act on its string stack or environment stack.

Betaload was intended to be a more useful version of Underload, in that you *could* maybe write actual useful programs if you tried hard enough.

## Example Programs:

All Underload programs will work with Betaload.

Truth-Machine:

    ()(((1)S:^):^)(1)((0)S)(0)R^

It reads in a string and, using the `R` instruction, replaces `1` with the code to infinitely print a 1, and 0 with code to print a 0. It then executes the resulting string.

Demonstration of environments:

    (foo)(bar)#>S(S)>{^}
    
This will push the two strings `foo` and `bar` to the stack. It then creates a new sub-environment (`#`) and moves the string `bar` to the stack of that environment (`>`). It outputs the string on top of the stack, which is now `foo`, and then sends the string `S` to the environment. Then, it executes that `S` using the frame of reference of that enviroment (`{^}`). The `S` outputs the value that is now on top of that environment's stack, `bar`. The output of this program is `foobar`.

One of the simplest things to do with environments is to use them for storing temporary values. For example, this Underload subprogram will rotate the top three values (with the element 2 spaces down moving to the top):

    a~a~*~a*^

This Betaload program does the same by using environments:

    >~<~
    
## Using the Interpreter

The interpreter is written in Python 3. To execute a file, run the interpreter as `python interpreter.py <path>`.
    
There is also a console mode. To start it, run the interpreter as `python interpreter.py -c`. You can enter in Betaload code to be executed:

    b > (S)
    #{(S)}
    b > :*
    #{(SS)}
    b > #>
    #{#{(SS)}}
    b > (foo):<^
    foofoo
    b > ;>;
    #
