signature EXPR = sig
  datatype expr
    = Height | K | S | I | Pop
    | Push of expr vector
    | Block of expr vector
    | App of expr vector
    | Comp of expr vector

  datatype bracket = Paren | Square | Angle | Brace
  exception UnmatchedClose of int * bracket
  exception UnmatchedOpen of int * bracket

  val parse: string -> expr vector

  type outstream
  val display: outstream * expr -> unit
end

signature EXPR_IO = STREAM_IO
  where type vector = string
  where type elem = char

functor Expr (Io: EXPR_IO): EXPR =
struct

  structure String : sig
    include STRING
    val iter: string * (int * char -> unit) -> unit
  end = struct
    open String
    fun iter (s, f) =
      let fun go (f, s, len: int, idx: int) =
        if Int.< (idx, len) then
          (f (idx, (sub (s, idx))); go (f, s, len, idx + 1))
        else ()
      in go (f, s, size s, 0) end
  end

  datatype expr
    = Height | K | S | I | Pop
    | Push of expr vector
    | Block of expr vector
    | App of expr vector
    | Comp of expr vector

  datatype bracket = Paren | Square | Angle | Brace
  exception UnmatchedClose of int * bracket
  exception UnmatchedOpen of int * bracket

  fun parse (s: string): expr vector =
    let
      type chain = expr list (* stored in reverse order *)
      val ch2v = vector o rev (* convert to a vector *)

      val brackets: (int * bracket * chain) list ref = ref []
      val curChain: chain ref = ref []

      fun push (idx, bracket) =
        (brackets := (idx, bracket, !curChain) :: !brackets;
         curChain := [])

      fun pop (idx, bracket, chainToExpr: chain -> expr) =
        case !brackets of
             (_, openBracket, prevChain) :: brackets' =>
               (if openBracket <> bracket then
                 raise UnmatchedClose (idx, bracket)
                else ();
                curChain := chainToExpr (!curChain) :: prevChain;
                brackets := brackets')
           | [] => raise UnmatchedClose (idx, bracket)

      fun handleChar (idx, c) = case c of
           #"[" => push (idx, Square)
         | #"(" => push (idx, Paren)
         | #"<" => push (idx, Angle)
         | #"{" => push (idx, Brace)
         | #"]" => pop (idx, Square, fn [] => Height
                                      | [e] => e
                                      | ch => App (ch2v ch))
         | #")" => pop (idx, Paren, fn [] => K
                                     | ch => Push (ch2v ch))
         | #">" => pop (idx, Angle, fn [] => S
                                     | [e] => e
                                     | ch => Comp (ch2v ch))
         | #"}" => pop (idx, Brace, fn [] => Pop
                                     | [Pop] => I
                                     | ch => Block (ch2v ch))
         | _ => ()
    in
      String.iter (s, handleChar);
      case !brackets of
          [] => ch2v (!curChain)
        | (idx, bracket, _) :: _ => raise UnmatchedOpen (idx, bracket)
    end

  type outstream = Io.outstream

  fun display (os, expr) =
    let
      fun displayP (os, openB, exprs, closeB) =
        (Io.output1 (os, openB);
         Vector.app (fn e => display (os, e)) exprs;
         Io.output1 (os, closeB))
    in
      case expr of
           Height => Io.output (os, "[]")
         | K      => Io.output (os, "()")
         | S      => Io.output (os, "<>")
         | I      => Io.output (os, "{{}}")
         | Pop    => Io.output (os, "{}")
         | Push  es => displayP (os, #"(", es, #")")
         | Block es => displayP (os, #"{", es, #"}")
         | App   es => displayP (os, #"[", es, #"]")
         | Comp  es => displayP (os, #"<", es, #">")
    end

end
