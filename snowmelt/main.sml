structure T = TextIO

fun showHelp (progName, isOk) =
  let
    val msg =
      (if isOk then ["snowmelt: ML interpreter for Flurry"] else []) @
      [ "Usage: " ^ progName ^ " [OPTIONS/FILE]..."
      , ""
      , "Options:"
      , "    -h"
      , "        Print this help message."
      , "    -i{format}, -i={format}"
      , "        Set the input format."
      , "    -o{format}, -o={format}"
      , "        Set the output format."
      , "    -r{format}, -r={format}"
      , "        Set the return value format."
      , "    -m"
      , "        Use magic impure functions for IO."
      , "    -c"
      , "        Treat the next argument as literal code to be interpreted."
      , "    -f"
      , "        Treat the next argument as a file name (the default)."
      , "    --"
      , "        Treat the next argument as a file name, even if it starts with `-`."
      , "    123"
      , "        Push the number to the stack above any input."
      , ""
      , "Formats:"
      , "    i   Church numeral displayed as decimal integer"
      , "    b   Church numeral displayed as raw byte         (i/o only)"
      , "    u   Church numeral displayed as UTF-8 codepoint  (i/o only)"
      , "    d   Debug representation                         (o/r only)"
      , "    n   Ignored"
      , "" ]
    val stream = if isOk then T.stdOut else T.stdErr
    val () = app (fn line => (
      T.output (stream, line);
      T.output1 (stream, #"\n"))) msg
    open OS.Process
  in
    exit (if isOk then success else failure)
  end

fun finalize (first, final) =
  let val res = first () handle err =>
    (final (); raise err)
  in final (); res end

structure E = Expr (TextIO.StreamIO)
structure V = Value (E)

type pgm = E.expr vector
datatype awaiting = Default | File | Literal

type config = {
  progName: string,
  pgms: pgm list ref,
  argInputs: int list ref,
  awaiting: awaiting ref
}

fun updateFlag (config: config) =
  fn #"h" => showHelp (#progName config, true)
   | #"c" => #awaiting config := Literal
   | #"f" => #awaiting config := Default
   | #"-" => #awaiting config := File
   | c =>
       (T.output (T.stdErr, "unrecognized flag: -" ^ str c);
        T.output1 (T.stdErr, #"\n");
        showHelp (#progName config, false))

fun readFile path =
  let val stream = T.openIn path in
    finalize (
      fn () => T.inputAll stream,
      fn () => T.closeIn stream
    )
  end
    handle IO.Io {cause = OS.SysErr (msg, _), ...} =>
      let
        val () = T.output (T.stdErr, "cannot read file '" ^ path ^ "': " ^ msg)
        val () = T.output1 (T.stdErr, #"\n")
        open OS.Process
      in exit failure end

fun pushProgram ({pgms, awaiting, ...}: config, code) =
  let val pgm = E.parse code in
    pgms := pgm :: !pgms;
    awaiting := Default
  end

fun pushArgInput ({argInputs, ...}: config, n: int) =
  argInputs := n :: !argInputs

fun update (config: config) arg =
  case !(#awaiting config) of
       Literal => pushProgram (config, arg)
     | File => pushProgram (config, readFile arg)
     | Default =>
         case Substring.getc (Substring.full arg) of
              SOME (#"-", rest) => Substring.app (updateFlag config) rest
            | NONE => ()
            (* treat `~1` as a filename, not a negative integer literal *)
            | SOME (#"~", rest) => pushProgram (config, readFile arg)
            | _ =>
                case Int.fromString arg of
                     SOME n => pushArgInput (config, n)
                   | NONE => pushProgram (config, readFile arg)

fun main (name, argv) =
  let val config = {
    progName  = name,
    pgms      = ref [],
    argInputs = ref [],
    awaiting  = ref Default
  } in
    app (update config) argv
  end
