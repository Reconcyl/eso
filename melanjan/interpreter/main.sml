type byte = Word8.word
signature PROGRAM_IO = sig
  val input: unit -> byte option;
  val output: byte -> unit
end

signature INTERPRETER = sig
  exception Error of string
  val interpret: string -> unit
end

functor MakeInterpreter(Io: PROGRAM_IO): INTERPRETER = struct

  exception Error of string
  fun malformed () = raise Error "malformed instruction"
  fun oob_write idx = raise Error ("write to invalid address: " ^ IntInf.toString idx)
  fun oob_jump idx = raise Error ("jumped to invalid address: " ^ IntInf.toString idx)
  fun bad_output c = raise Error ("output invalid byte value: " ^ IntInf.toString c)

  fun input (): IntInf.int =
    case Io.input () of
         SOME n => IntInf.fromInt (Word8.toInt n)
       | NONE => ~1

  fun output (c: IntInf.int): unit =
    if c < 256
    then Io.output (Word8.fromInt (IntInf.toInt c))
    else bad_output c

  type state = {
    reg_a: IntInf.int ref,
    reg_b: IntInf.int ref,
    reg_c: IntInf.int ref,
    ip: int ref,
    program: IntInf.int array
  }

  fun read_indirect (
    state: state,
    reg_selector: state -> IntInf.int ref
  ): IntInf.int =
    let
      val r = !(reg_selector state)
      val program = #program state
    in
      if r < 0 then ~1 else
      if r >= IntInf.fromInt (Array.length program) then 32 else
      Array.sub (program, IntInf.toInt r)
    end

  fun update_direct (
    state: state,
    reg_selector: state -> IntInf.int ref,
    right_value: IntInf.int
  ): unit =
    let val r = reg_selector state in
      r := right_value - (!r)
    end

  fun update_indirect (
    state: state,
    reg_selector: state -> IntInf.int ref,
    right_value: IntInf.int
  ): unit =
    let
      val addr = !(reg_selector state)
      val program = #program state
    in
      if (0 <= addr) andalso (addr < IntInf.fromInt (Array.length program)) then
        let
          val addr = IntInf.toInt addr
          val left_value = Array.sub (program, addr)
        in
          Array.update (program, addr, right_value - left_value)
        end
      else oob_write(addr)
    end

  fun update_ip (
    state: state,
    right_value: IntInf.int
  ): unit =
    let
      val old_ip = !(#ip state)
      val right_value =
        (IntInf.toInt right_value)
        handle Overflow => oob_jump (right_value - IntInf.fromInt old_ip)
    in
      #ip state := right_value - old_ip
    end

  fun perform (state: state, left: char, right: char) =
    let val right_value =
      case right of #"1" => 1
                  | #"i" => IntInf.fromInt (!(#ip state))
                  | #"o" => input ()
                  | #"a" => !(#reg_a state)
                  | #"b" => !(#reg_b state)
                  | #"c" => !(#reg_c state)
                  | #"A" => read_indirect (state, #reg_a)
                  | #"B" => read_indirect (state, #reg_b)
                  | #"C" => read_indirect (state, #reg_c)
                  | _ => malformed ()
    in
      case left of #"i" => update_ip (state, right_value)
                 | #"o" => output right_value
                 | #"a" => update_direct (state, #reg_a, right_value)
                 | #"b" => update_direct (state, #reg_b, right_value)
                 | #"c" => update_direct (state, #reg_c, right_value)
                 | #"A" => update_indirect (state, #reg_a, right_value)
                 | #"B" => update_indirect (state, #reg_b, right_value)
                 | #"C" => update_indirect (state, #reg_c, right_value)
                 | _ => malformed ()
    end

  fun getCmd (c: IntInf.int) =
    if 0 <= c andalso c < 256
    then chr (IntInf.toInt c)
    else malformed ()

  fun step (state: state) =
    let
      val ip = !(#ip state)
      val program = #program state
      val _ = if 0 <= ip andalso ip + 1 < Array.length program then () else
        oob_jump (IntInf.fromInt ip)
      val left = Array.sub (program, ip)
      val right = Array.sub (program, ip + 1)
      val () = perform (state, getCmd left, getCmd right)
      val ip = ip + 2
    in
      if ip = Array.length program then () else
        (#ip state := ip; step(state))
    end

  fun interpret s =
    let val state =
      { reg_a = ref 0,
        reg_b = ref 0,
        reg_c = ref 0,
        ip = ref 0,
        program = Array.tabulate (
          String.size s,
          fn i => IntInf.fromInt (ord (String.sub (s, i))))
      }
    in step(state) end

end

signature MAIN = sig
  val main: string * string list -> OS.Process.status
end

structure Main: MAIN = struct

  structure Stdio : PROGRAM_IO = struct
    (* it would be better to use BinIO here, but stdin and stdout
     * aren't provided as BinIO streams in the standard library *)
    fun input () =
      let val c = TextIO.input1 TextIO.stdIn
      in Option.map (Word8.fromInt o ord) c end
    fun output b =
      TextIO.output1 (TextIO.stdOut, chr (Word8.toInt b))
  end

  structure Interpreter = MakeInterpreter(Stdio)

  fun main (_, [filename]) =
    let
      val stream = TextIO.openIn filename
      val program = TextIO.inputAll stream
      val () = TextIO.closeIn stream
    in
      (Interpreter.interpret program; OS.Process.success)
      handle (Interpreter.Error msg) =>
        (TextIO.output (TextIO.stdErr, msg);
         TextIO.output1 (TextIO.stdErr, #"\n");
         OS.Process.failure)
    end

    | main (program_name, args) =
       (TextIO.output (TextIO.stdErr, "usage: " ^ program_name ^ " [file]\n");
        if null args then OS.Process.success else OS.Process.failure)

end

val () =
  if MLton.isMLton then
    OS.Process.exit (Main.main
      (CommandLine.name (), CommandLine.arguments ()))
  else ()
