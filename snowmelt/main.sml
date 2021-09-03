structure String :> sig
  include STRING where type string = string
                 where type char = char
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

fun repeat (0, f, x) = x
  | repeat (n, f, x) = repeat (n - 1, f, f x)

datatype expr
  = EHeight | EK | ES | EI | EPop
  | EPush of expr vector
  | EBlock of expr vector
  | EApp of expr vector
  | EComp of expr vector

datatype value
  = K
  | K1 of value
  | S
  | S1 of value
  | S2 of value * value
  | I
  | Comp of value * value
  | Block of expr vector
  | ChurchSucc
  | ChurchNum of int
  | ChurchNum1 of int * value
  | TestN of int
  | TestSucc
  | TestFail

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
       | #"]" => pop (idx, Square, fn [] => EHeight
                                    | [e] => e
                                    | ch => EApp (ch2v ch))
       | #")" => pop (idx, Paren, fn [] => EK
                                   | ch => EPush (ch2v ch))
       | #">" => pop (idx, Angle, fn [] => ES
                                   | [e] => e
                                   | ch => EComp (ch2v ch))
       | #"}" => pop (idx, Brace, fn [] => EPop
                                   | [EPop] => EI
                                   | ch => EBlock (ch2v ch))
       | _ => ()
  in
    String.iter (s, handleChar);
    case !brackets of
        [] => ch2v (!curChain)
      | (idx, bracket, _) :: _ => raise UnmatchedOpen (idx, bracket)
  end

type ctx = {
  stack: (int * value list) ref,
  stackStrictMode: bool
}

fun push ({stack, ...}: ctx, x) =
  let val (h, xs) = !stack in stack := (h + 1, x :: xs) end

fun pop {stack, stackStrictMode = strict} =
  case !stack of
       (_, []) => if strict then TestFail else I
     | (h, (x :: xs)) => (stack := (h - 1, xs); x)

fun stackHeight {stackStrictMode = false, stack} = NONE
  | stackHeight {stack = ref (h, _), ...} = SOME h

fun apply (ctx, f, x) =
  case (f, x) of
       (K, x) => K1 x
     | (K1 x, _) => x
     | (S, K) => K1 I (* this transformation is not always correct! *)
     | (S, Comp (S, K)) => ChurchSucc
     | (S, x) => S1 x
     | (S1 x, y) => S2 (x, y)
     | (S2 (x, y), z) =>
         let
           val xz = apply (ctx, x, z)
           val yz = apply (ctx, y, z)
         in apply (ctx, xz, yz) end
     | (I, x) => x
     | (Comp (f, g), x) => apply (ctx, f, apply (ctx, g, x))
     | (Block exprs, x) => (push (ctx, x); reduce (ctx, exprs))
     | (ChurchSucc, K1 I) => ChurchNum 1
     | (ChurchSucc, I) => ChurchNum 2
     | (ChurchSucc, ChurchNum n) => ChurchNum (n + 1)
     | (ChurchSucc, x) => S2 (Comp (S, K), x)
     | (ChurchNum n, f) => ChurchNum1 (n, f)
     | (ChurchNum1 (n, ChurchSucc), K1 I) => ChurchNum n
     | (ChurchNum1 (n, ChurchSucc), I) => ChurchNum (n + 1)
     | (ChurchNum1 (n, ChurchSucc), ChurchNum m) => ChurchNum (n + m)
     | (ChurchNum1 (n, f), x) => repeat (n, fn x => apply (ctx, f, x), x)
     | (TestN _, _) => TestFail
     | (TestSucc, TestN i) => (TestN (i + 1))
     | (TestSucc, _) => TestFail
     | (TestFail, _) => TestFail

and reduce (ctx, exprs) =
  let
    fun step (expr, f) =
      let val x = eval (ctx, expr)
      in apply (ctx, f, x) end
  in Vector.foldl step I exprs end

and reduceComp (ctx, exprs): value =
  let
    (* is applying this term to an argument guaranteed not to have side effects? *)
    fun pure K = true
      | pure (K1 _) = true
      | pure S = true
      | pure (S1 _) = true
      | pure (S2 _) = false (* be conservative *)
      | pure I = true
      | pure (Comp (f, g)) = pure f andalso pure g
      | pure (Block _) = false
      | pure ChurchSucc = true (* S1 (Comp (S, K)) *)
      | pure (ChurchNum n) = true
      | pure (ChurchNum1 (0, _)) = false
      | pure (ChurchNum1 (_, f)) = pure f
      | pure (TestN _) = true
      | pure TestSucc = true
      | pure TestFail = true

    (* compose f with the results of all expressions in the input vector starting from gIdx *)
    fun composeAt (f, gIdx) =
      if gIdx < Vector.length exprs then
        let
          val g = eval (ctx, Vector.sub (exprs, gIdx))

          fun reduceTo x = composeAt (x, gIdx + 1)
          fun continue () = Comp (f, reduceTo g)
        in
          case (f, g) of
               (I, _) => reduceTo g
             | (_, I) => reduceTo f
             | (ChurchNum 1, _) => reduceTo g
             | (_, ChurchNum 1) => reduceTo f
             | (K1 _, g) => if pure g then reduceTo f else continue ()
             | (ChurchNum 0, g) => if pure g then reduceTo f else continue ()
             | (ChurchNum n, K1 _) => reduceTo g (* valid because n must be >0 *)
             | (ChurchNum n, ChurchNum m) => reduceTo (ChurchNum (n * m))
             | _ => continue ()
        end
      else f
  in
    composeAt (I, 0)
  end

and eval (ctx, expr) =
  case expr of
       EHeight   =>
       (case stackHeight ctx of
             SOME n => ChurchNum n
           | NONE => TestFail)
     | EK        => K
     | ES        => S
     | EI        => I
     | EPop      => pop ctx
     | EPush es  => let val res = reduce (ctx, es) in (push (ctx, res); res) end
     | EBlock es => Block es
     | EApp es   => reduce (ctx, es)
     | EComp es  => reduceComp (ctx, es)
