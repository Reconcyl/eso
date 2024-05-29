## Lazy K programs and tools

I am using a slightly modified version of [Lazier](https://github.com/msullivan/LazyK/blob/master/lazier.scm), which is released under the GPL.

`main.scm` implements an answer to the codegolf.SE challenge "[Confound my attempts to solve the halting problem](https://codegolf.stackexchange.com/a/248185)." It depends on a slightly modified version of [Lazier](https://github.com/msullivan/LazyK/blob/master/lazier.scm), which is released under the GPL.

`examples/` contains a number of Lazy K programs, including an existing "Hello World" program (1059 bytes) and a new one (555 bytes).

`Find3.hs` is a brute forcer for SKI terms with desired properties. It can be compiled using the provided Makefile.

`honeycomb` is the beginnings of a Lazy K interpreter (specifically, it properly implements the Unlambda subset). Currently there is a prototype in Haskell, but I intend to ultimately rewrite it in Zig or similar.
