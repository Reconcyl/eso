structure T = TextIO

fun err stream msg =
  (T.output (stream, msg);
   T.output1 (stream, #"\n"))

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
    val () = app (err stream) msg
    open OS.Process
  in
    exit (if isOk then success else failure)
  end

val err = err T.stdErr
fun errExit msg = (err msg; OS.Process.exit OS.Process.failure)

fun finalize (first, final) =
  let val res = first () handle err =>
    (final (); raise err)
  in final (); res end

structure E = Expr (TextIO.StreamIO)
structure V = Value (E)

type pgm = E.expr vector
datatype awaiting = Default | File | Literal

datatype ioFormat = FInt | FByte | FUnicode | FDebug | FNone | FDefault

fun mkReader FInt =
      (fn () =>
         let
           open Char
           fun next2 n =
             case T.input1 T.stdIn of
                  SOME c =>
                    if isDigit c then
                      next2 ((n * 10 + (ord c - ord #"0"))
                        handle Overflow => errExit "input number is too large")
                    else SOME n
                | NONE => SOME n
           fun next1 () =
             case T.input1 T.stdIn of
                  NONE => NONE
                | SOME c =>
                    if isDigit c then
                      next2 (ord c - ord #"0")
                    else
                      next1 ()
         in next1 () end
      )
  | mkReader FByte =
      (fn () => Option.map Char.ord (T.input1 T.stdIn))
  | mkReader FUnicode =
      (fn () =>
         case T.input1 T.stdIn of
              NONE => NONE
            | SOME chr => SOME
                let
                  open Word32
                  infix andb orb <<
                  fun fail () = errExit "input is not valid UTF-8"
                  fun getContByte () =
                    case Option.map (fromInt o Char.ord) (T.input1 T.stdIn) of
                         NONE => fail ()
                       | SOME c =>
                           if (c andb 0wxC0) = 0wx80 then c andb 0wx3F else fail ()

                  val a = fromInt (Char.ord chr)
                  fun validate n =
                    if 0wxD800 <= n andalso n < 0wxE000 then fail () (* surrogate *) else n

                in toInt (
                  if (a andb 0wx80) = 0w0 then
                    a (* ascii *)
                  else validate (
                    case getContByte () of b =>
                    let val b = getContByte () in
                      if (a andb 0wxE0) = 0wxC0 then
                        b orb ((a andb 0wx1F) << 0w6)
                      else
                        let val c = getContByte () in
                          if (a andb 0wxF0) = 0wxE0 then
                            c orb (b << 0w6) orb ((a andb 0wx0) << 0w12)
                          else if (a andb 0wxF8) = 0wxF0 then
                            let val d = getContByte () in
                              d orb (c << 0w6) orb (b << 0w12) orb ((a andb 0wx07) << 0w18)
                            end
                          else fail ()
                        end
                    end
                  )
                ) end
      )
  | mkReader FDebug =
      errExit "cannot input using debug representation"
  | mkReader FNone =
      (fn () => NONE)
  | mkReader FDefault = raise Fail "shouldn't pass this"

type config = {
  progName: string,
  pgms: pgm list ref,
  anyLiterals: bool ref,
  argInputs: int list ref,
  awaiting: awaiting ref,

  iFormat: ioFormat ref,
  oFormat: ioFormat ref,
  rFormat: ioFormat ref,
  activeFormatParam: ioFormat ref option ref
}

fun resolveDefaults (config: config) =
  let
    val anyLiterals = !(#anyLiterals config)
    fun replaceDefault (format, def1, def2) =
      case !format of
           FDefault => format := (if anyLiterals then def1 else def2)
         | _ => ()
  in
    replaceDefault (#iFormat config, FNone, FInt);
    replaceDefault (#oFormat config, FDebug, FInt);
    replaceDefault (#rFormat config, FDebug, FNone)
  end

fun validate (config: config) =
  (* at least one program must be passed *)
  (if null (!(#pgms config)) then showHelp (#progName config, true) else ();
   (* -- and -c must be followed by another argument *)
   case !(#awaiting config) of
        Default => ()
      | File => (err "expected file name"; showHelp (#progName config, false))
      | Literal => (err "expected code"; showHelp (#progName config, false));
   (* `-i=`, etc. must be followed by an IO format *)
   if isSome (!(#activeFormatParam config)) then
     (err "expected format"; showHelp (#progName config, false))
   else ())

fun updateFlag (config: config) c =
  case !(#activeFormatParam config) of
       NONE =>
         (case c of
               #"h" => showHelp (#progName config, true)
             | #"i" => #activeFormatParam config := SOME (#iFormat config)
             | #"o" => #activeFormatParam config := SOME (#oFormat config)
             | #"r" => #activeFormatParam config := SOME (#rFormat config)
             | #"c" => #awaiting config := Literal
             | #"f" => #awaiting config := Default
             | #"-" => #awaiting config := File
             | _ =>
                 (err ("unrecognized flag: -" ^ str c);
                  showHelp (#progName config, false)))
     | SOME r =>
         let fun clear () = #activeFormatParam config := NONE in
           case c of
                #"=" => ()
              | #"i" => clear (r := FInt)
              | #"b" => clear (r := FByte)
              | #"u" => clear (r := FUnicode)
              | #"d" => clear (r := FDebug)
              | #"n" => clear (r := FNone)
              | _ =>
                  (err ("unrecognized I/O format: " ^ str c);
                   showHelp (#progName config, false))
         end

fun readFile path =
  let val stream = T.openIn path in
    finalize (
      fn () => T.inputAll stream,
      fn () => T.closeIn stream
    )
  end
    handle IO.Io {cause = OS.SysErr (msg, _), ...} =>
      errExit ("cannot read file '" ^ path ^ "': " ^ msg)

fun pushProgram ({pgms, awaiting, ...}: config, code, kind, arg) =
  let val pgm = E.parse code in
    pgms := pgm :: !pgms;
    awaiting := Default
  end
    handle E.Unmatched (pos, role, bracket) =>
      let
        val bracket =
          case (role, bracket) of
               (E.Open, E.Paren)   => "("
             | (E.Open, E.Square)  => "["
             | (E.Open, E.Angle)   => "<"
             | (E.Open, E.Brace)   => "{"
             | (E.Close, E.Paren)  => ")"
             | (E.Close, E.Square) => "]"
             | (E.Close, E.Angle)  => ">"
             | (E.Close, E.Brace)  => "}"
      in
        errExit ("cannot parse " ^ kind ^ " '" ^ arg ^ "': unmatched '"
          ^ bracket ^ "' at position " ^ Int.toString (pos + 1) ^ "\n")
      end

fun pushArgInput ({argInputs, ...}: config, n: int) =
  argInputs := n :: !argInputs

fun update (config: config) arg =
  let
    fun treatArgAsProgram () =
      (#anyLiterals config := true;
       pushProgram (config, arg, "argument", arg))
    fun treatArgAsFile () =
      pushProgram (config, readFile arg, "file", arg)
  in
    case !(#awaiting config) of
         Literal => treatArgAsProgram ()
       | File => treatArgAsFile ()
       | Default =>
           case Substring.getc (Substring.full arg) of
                SOME (#"-", rest) => Substring.app (updateFlag config) rest
              | NONE => ()
              (* treat `~1` as a filename, not a negative integer literal *)
              | SOME (#"~", rest) => treatArgAsFile ()
              | _ =>
                  case
                    Int.fromString arg
                      handle Overflow => errExit ("integer " ^ arg ^ " is too large")
                  of SOME n => pushArgInput (config, n)
                   | NONE => treatArgAsFile ()
  end

fun main (name, argv) =
  let val config = {
    progName    = name,
    pgms        = ref [],
    anyLiterals = ref false,
    argInputs   = ref [],
    awaiting    = ref Default,
    iFormat = ref FDefault,
    oFormat = ref FDefault,
    rFormat = ref FDefault,
    activeFormatParam = ref NONE
  } in
    app (update config) argv;
    resolveDefaults config;
    validate config
  end
