# 9f8

A JavaScript compiler for the
[9f87m4atttaaaou;](https://esolangs.org/wiki/9f87m4atttaaaou;) programming
language. Specifically, it generates self-contained JS code, which can either
be executed with `eval` or written to a file. The compiler takes the form of a
single anonymous function which should be executed in the browser by passing a
string. IO uses `console.log` and `prompt` by default, but this can be
configured via another arguments to the function.

While the compiler's source code has not been uploaded directly, it is
available in two forms:

- As encrypted data string in the program `decrypt_compiler.js`. If the correct
  key is passed via environment variable, this script will dump the original
  compiler source to STDOUT.
- In minified form as `min_compiler.js`. This is not obfuscated at all beyond
  basic size optimizations, so the interested reader should be able to reverse
  engineer it without too much trouble.
