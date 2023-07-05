signature MAIN = sig
  val main : string * string list -> OS.Process.status
end

structure Main : MAIN = struct

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
        , "    -I{format}, -I={format}"
        , "        Set the input format."
        , "    -O{format}, -O={format}"
        , "        Set the output format."
        , "    -R{format}, -R={format}"
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
        , "    b   Church numeral displayed as raw byte         (I/O only)"
        , "    u   Church numeral displayed as UTF-8 codepoint  (I/O only)"
        , "    d   Debug representation                         (O/R only)"
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
    (first () handle err =>
      (final (); raise err))
    before (final ())

  structure Syntax = Syntax (TextIO.StreamIO)
  structure Value = Value (
    structure Io = TextIO.StreamIO;
    structure Syntax = Syntax
  )

  type pgm = Expr.expr vector
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
                      let val b = getContByte () in
                        if (a andb 0wxE0) = 0wxC0 then
                          b orb ((a andb 0wx1F) << 0w6)
                        else
                          let val c = getContByte () in
                            if (a andb 0wxF0) = 0wxE0 then
                              c orb (b << 0w6) orb ((a andb 0wx0F) << 0w12)
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

  fun writeCodePoint (code: Word32.word) =
    let
      open Word32
      infix andb orb >>
      val byte = print o str o Char.chr o toInt
    in
      if code < 0wx80 then
        byte code
      else if code < 0wx0800 then
        (byte (((code >> 0w6)  andb 0wx1F) orb 0wxC0);
         byte (((code >> 0w0)  andb 0wx3F) orb 0wx80))
      else if code < 0wx010000 then
        (byte (((code >> 0w12) andb 0wx0F) orb 0wxE0);
         byte (((code >> 0w6)  andb 0wx3F) orb 0wx80);
         byte (((code >> 0w0)  andb 0wx3F) orb 0wx80))
      else if code < 0wx110000 then
        (byte (((code >> 0w18) andb 0wx07) orb 0wxF0);
         byte (((code >> 0w12) andb 0wx3F) orb 0wx80);
         byte (((code >> 0w6)  andb 0wx3F) orb 0wx80);
         byte (((code >> 0w0)  andb 0wx3F) orb 0wx80))
      else
        () (* invalid codepoints are silently ignored *)
    end

  fun intOnly f v = case Value.toInt v of SOME n => f n | NONE => ()

  fun mkWriter FInt =
        intOnly (fn i => (print (Int.toString i); print " "))
    | mkWriter FByte =
        intOnly (fn i => print (str (Char.chr (i mod 256))))
    | mkWriter FUnicode =
        intOnly (writeCodePoint o Word32.fromInt)
    | mkWriter FDebug =
        (fn v => (T.output (T.stdErr, "Output: ");
                  Value.display (T.getOutstream T.stdErr, v);
                  T.output1 (T.stdErr, #"\n")))
    | mkWriter FNone = ignore
    | mkWriter FDefault = raise Fail "shouldn't pass this"

  fun mkReturner FInt =
        intOnly (print o Int.toString)
    | mkReturner FByte =
        errExit "cannot format return value as byte"
    | mkReturner FUnicode =
        errExit "cannot format return value as unicode character"
    | mkReturner FDebug =
        (fn v => (T.output (T.stdErr, "Return: ");
                  Value.display (T.getOutstream T.stdErr, v);
                  T.output1 (T.stdErr, #"\n")))
    | mkReturner FNone = ignore
    | mkReturner FDefault = raise Fail "shouldn't pass this"

  type config = {
    progName: string,
    pgms: pgm list ref,
    anyLiterals: bool ref,
    argInputs: int list ref,
    awaiting: awaiting ref,

    iFormat: ioFormat ref,
    oFormat: ioFormat ref,
    rFormat: ioFormat ref,
    activeFormatParam: ioFormat ref option ref,
    magicIo: bool ref
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
               | #"I" => #activeFormatParam config := SOME (#iFormat config)
               | #"O" => #activeFormatParam config := SOME (#oFormat config)
               | #"R" => #activeFormatParam config := SOME (#rFormat config)
               | #"m" => #magicIo config := true
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
    let val pgm = Syntax.parse code in
      pgms := pgm :: !pgms;
      awaiting := Default
    end
      handle Syntax.Unmatched (pos, role, bracket) =>
        let
          open Syntax
          val bracket =
            case (role, bracket) of
                 (Open, Paren)   => "("
               | (Open, Square)  => "["
               | (Open, Angle)   => "<"
               | (Open, Brace)   => "{"
               | (Close, Paren)  => ")"
               | (Close, Square) => "]"
               | (Close, Angle)  => ">"
               | (Close, Brace)  => "}"
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

  fun runWith (config: config) =
    let
      val ctx = Value.init ()
      val inputFn = mkReader (!(#iFormat config))
      val outputFn = mkWriter (!(#oFormat config))
      val returnFn = mkReturner (!(#rFormat config))
      val magic = !(#magicIo config)

      (* push additional stack values *)
      fun pushArgInputs () =
        app (fn i => Value.push (ctx, Value.fromInt i)) (rev (!(#argInputs config)))

      val () =
        if magic then
          (pushArgInputs ();
           Value.push (ctx, Value.magicInput inputFn);
           Value.push (ctx, Value.magicOutput outputFn))
        else
          (* read all inputs and push them to the stack *)
          let fun go () =
            case inputFn () of
                 NONE => ()
               | SOME i => (Value.push (ctx, Value.fromInt i); go ())
          in
            go ();
            pushArgInputs ()
          end
      (* run the program *)
      val aggregatePgm = vector (map (fn pgm => Expr.App pgm) (!(#pgms config)))
      val result = Value.reduce (ctx, aggregatePgm)
    in
      if magic then () else let
        (* create a list of all outputs *)
        fun go os =
          case Value.pop ctx of
               NONE => os
             | SOME v => go (v :: os)
        val outputs = go []
      in
        (* write the return value *)
        app outputFn outputs;
        (* write the outputs *)
        returnFn result
      end
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
      activeFormatParam = ref NONE,
      magicIo = ref false
    } in
      app (update config) argv;
      resolveDefaults config;
      validate config;
      runWith config;
      OS.Process.success
    end

end
