module type Fsm = {
  type st
  type tr 'a
  -- tr a should be isomorphic to (st -> a)
  val mktr 'a : (st -> a) -> tr a
  val untr 'a : tr a -> (st -> a)
}

module Run (F : Fsm) : {
  val run [n] 'c : (c -> F.st -> F.st) -> F.st -> [n]c -> [n]F.st
} = {
  open F
  type s2s = tr st
  def identity : s2s = mktr (\s -> s)
  def compose (a : s2s) (b : s2s) : s2s = mktr (\s -> untr b (untr a s))
  def run f init xs =
    xs |> map (\c -> mktr (f c))
       |> scan compose identity
       |> map (\t -> untr t init)
}

module Syn = {
  module M = {
    type st    = #o | #q | #qq | #c
    type tr 'a = { o : a, q : a, qq : a, c : a}
    def mktr 'a (f : st -> a) = { o = f #o, q = f #q, qq = f #qq, c = f #c }
    def untr 'a (t : tr a)    = \(st : st) ->
      match st
        case #o  -> t.o
        case #q  -> t.q
        case #qq -> t.qq
        case #c  -> t.c
  }
  open M
  module R = Run(M)
  def categorize (b : u8) (st : st) : st =
    match (st, b)
      case (#o,  '{')  -> #c
      case (#o,  '\'') -> #q
      case (#o,  '"')  -> #qq
      case (#o,  _)    -> #o
      case (#q,  _)    -> #o
      case (#qq, '"')  -> #o
      case (#qq, _)    -> #qq
      case (#c,  '}')  -> #o
      case (#c,  _)    -> #c
  def lex = R.run categorize #o
}

def filter_map 'a 'b (f : a -> (bool, b)) (xs : []a) : []b =
  xs |> map f |> filter (\(b, _) -> b) |> map (\(_, y) -> y)

type option 'a = #none | #some a

def compile [n] (code: [n]u8) (states: [n]Syn.st) : []u8 =
  -- count closing braces as part of the comment
  let states = tabulate n (\i -> if i > 0 && states[i - 1] == #c then #c else states[i]) in
  -- remove comments
  let pgm = zip3 code states (iota n) |> filter (\(_, st, _) -> st != #c)
  -- find positions of all opening and closing double quotes
  let double_quotes = pgm |> filter (\(c, _, _) -> c == '"') in
  let [q] opening: [q]i64
              = double_quotes |> filter_map (\(_, st, i) -> (st == #qq, i)) in
  let closing = double_quotes |> filter_map (\(_, st, i) -> (st == #o,  i)) :> [q]i64 in
  let string_lengths = map2 (\start end_ -> end_ - start - 1) opening closing in
  let string_offsets = scan (+) 0 string_lengths |> \os -> map (\o -> o - os[0]) os in
  let [k] (pgm: [k](u8, Syn.st, i64), string_data) = pgm |> partition (\(c, st, _) -> st != #qq) in
  let string_data = string_data |> filter_map (\(c, _, _) -> (c == '"', c)) in
  -- find positions of all character literals
  let quote_mask = pgm |> map (\(_, st, _) -> st == #q) |> rotate (-1) in
  -- construct aux array
  let aux = map2 (\(c, _, _) is_quote -> if is_quote then i64.u8 c else 0) pgm quote_mask in
  let pgm = zip3 pgm (rotate 1 aux) quote_mask
            |> filter_map (\((c, _, _), aux, after_quote) -> (!after_quote, (c, aux))) in
  -- construct scan for integer literal parsing
  let int_data : [](option (i64, i64)) =
      pgm |> map (\(c, _) -> if '0' <= c && c <= '9' then #some (10, i64.u8 (c - '0')) else #none)
          |> scan (\o1 o2 -> match (o1, o2)
                               case (_,              #none         ) -> #none
                               case (#none,          #some t       ) -> #some t
                               case (#some (p1, v1), #some (p2, v2)) -> #some (p1 * p2, v1 * p2 + v2)
                  ) #none
  in
  -- fold int data into aux array
  let pgm = zip3 pgm int_data (indices int_data)
            |> filter_map (\((c, aux), int_status, i) ->
                 match int_status
                   case #none        ->   (true,  (c, aux))
                   case #some (_, v) ->
                     if i + 1 >= length int_data then
                                          (false, (0, 0))
                     else
                       match int_data[i + 1]
                         case #none    -> (true,  ('0', v))
                         case #some _  -> (false, (0, 0)))
  in
  pgm |> map (\(c, _) -> c)

def main (code: []u8) : (bool, []u8) =
  let states = Syn.lex code in
  let final = if null states then #o else last states in
  match final
    case #q  -> (false, "unterminated character literal")
    case #qq -> (false, "unterminated string")
    case #c  -> (false, "unterminated comment")
    case #o  ->
      -- are there unmatched close braces?
      if or (map2 (\c st -> c == '}' && st == #o) code states) then
        (false, "unmatched end of comment")
      else
        (true, compile code states)
