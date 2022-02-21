# Pair

A compiler for the language [Pair](https://esolangs.org/wiki/Pair).

It targets OCaml since it's also a functional language and can implement Pair's semantics fairly easily. It's written in [Tamsin](github.com/catseye/Tamsin/) since it's convenient for writing parsers. The result is, in my opinion, simpler and more maintainable than existing implementations of Pair.

Note that due to the limitations of Tamsin, you might run into recursion limits while trying to compile something with a very large number of declarations. Also, error messages due to incorrect syntax are confusing since they tend to cause it to backtrack all the way to the start.