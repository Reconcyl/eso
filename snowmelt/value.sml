signature VALUE = sig
  type value
  type ctx

  val init: unit -> ctx
  val push: ctx * value -> unit
  val reduce: ctx * Expr.expr vector -> value

  val fromInt: int -> value
  val toInt: value -> int option

  type outstream
  val display: outstream * value -> unit
end

functor Value (
  structure Io: SYNTAX_IO;
  structure Syntax: SYNTAX where type outstream = Io.outstream
) :> VALUE where type outstream = Io.outstream =
struct

  (* apply `f` to `x` a given number of times *)
  fun repeat (0, f, x) = x
    | repeat (n, f, x) = repeat (n - 1, f, f x)

  structure E = Expr

  datatype value
    = K
    | K1 of value
    | S
    | S1 of value
    | S2 of value * value
    | I
    | Comp of value * value
    | Block of E.expr vector
    | ChurchSucc
    | ChurchNum of int
    | ChurchNum1 of int * value
    | TestN of int
    | TestSucc
    | TestFail

  type ctx = { stack: (int * value list) ref }

  fun init () = {stack = ref (0, [])}

  fun push ({stack}, x) =
    let val (h, xs) = !stack in stack := (h + 1, x :: xs) end

  fun pop {stack} =
    case !stack of
         (_, []) => I
       | (h, (x :: xs)) => (stack := (h - 1, xs); x)

  fun stackHeight ({stack}: ctx) = #1 (!stack)

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
       | (ChurchNum n, ChurchNum1 (m, f)) => ChurchNum1 (n * m, f)
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
         E.Height   => ChurchNum (stackHeight ctx)
       | E.K        => K
       | E.S        => S
       | E.I        => I
       | E.Pop      => pop ctx
       | E.Push es  => let val res = reduce (ctx, es) in push (ctx, res); res end
       | E.Block es => Block es
       | E.App es   => reduce (ctx, es)
       | E.Comp es  => reduceComp (ctx, es)

  fun fromInt n = ChurchNum n

  fun toInt value =
    let fun fallback () =
      let
        val stack = ref (0, [])
        val ctx = {stack = stack}
      in
        case apply (ctx, apply (ctx, value, TestSucc), TestN 0) of
             TestN n => if #1 (!stack) = 0 then SOME n else NONE
           | _ => NONE
      end
    in
      case value of
           K => NONE
         | K1 I => SOME 0
         | K1 _ => fallback ()
         | S => NONE
         | S1 _ => fallback ()
         | S2 _ => fallback ()
         | I => SOME 1
         | Comp _ => fallback ()
         | Block _ => fallback ()
         | ChurchSucc => NONE
         | ChurchNum n => SOME n
         | ChurchNum1 (0, _) => SOME 1
         | ChurchNum1 _ => fallback ()
         | TestN _ => NONE
         | TestSucc => NONE
         | TestFail => NONE
    end

  type outstream = Io.outstream
  fun display (os, ChurchNum n) = Io.output (os, Int.toString n)
    | display (os, value) =
        let
          fun s str = Io.output (os, str)
          fun c chr = Io.output1 (os, chr)

          datatype Pos = Hd | Arg | Cmp

          fun go (_,   K)          = s "()"
            | go (Hd,  K1 x)       = (s "()"; go (Arg, x))
            | go (_,   S)          = s "<>"
            | go (Hd,  S1 x)       = (s "<>"; go (Arg, x))
            | go (Hd,  S2 (x,y))   = (s "<>"; go (Arg, x); go (Arg, y))
            | go (_,   I)          = s "{{}}"
            | go (Cmp, Comp (x,y)) = (go (Cmp, x); go (Cmp, y))
            | go (_,   Comp (x,y)) = (c #"<"; go (Cmp, x); go (Cmp, y); c #">")
            | go (_,   Block es)   =
                (c #"{";
                 Vector.app (fn e => Syntax.display (os, e)) es;
                 c #"}")

            | go (Hd,  ChurchSucc)       = s "<><<>()>"
            | go (_,   ChurchNum n)      = s (Int.toString n)
            | go (Hd,  ChurchNum1 (n,f)) = (s (Int.toString n); c #" "; go (Arg, f))

            | go (_, TestN n)  = s ("[!" ^ Int.toString n ^ "]")
            | go (_, TestSucc) = s "[!succ]"
            | go (_, TestFail) = s "[!fail]"

            | go (Arg, x) = (c #"["; go (Hd, x); c #"]")
            | go (Cmp, x) = (c #"["; go (Hd, x); c #"]")

        in
          case toInt value of
               SOME n => Io.output (os, "(" ^ Int.toString n ^ ") ")
             | NONE => ();
          go (Hd, value)
        end

end
