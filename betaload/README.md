# betaload-lang

Betaload is a derivative of Underload. It is backwards compatible with Underload, and it was designed to be more useful.

One of the biggest changes to Betaload is the concept of *environments*. An environment is a space where you can execute commands. In Underload, there is only one environment, which is the environment containing the main stack. In Betaload, this is just the *root environment*. Every environment has two stacks: a stack of strings, and a stack of sub-environments. Environments can be dynamically created and data can be sent to and from them.

Each Betaload environment can be in one of two modes: S-mode or E-mode. The environment's mode determines whether certain commands act on its string stack or environment stack.

Betaload was intended to be a more useful version of Underload, in that you *could* maybe write actual useful programs if you tried hard enough.
