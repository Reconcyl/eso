import System.Environment (getArgs)
import Data.List (transpose, intercalate)
import Debug.Trace

data SKI = S | K | I | Ap SKI SKI deriving (Show)

parse :: String -> SKI
parse s = res where
  [res] = foldr step [] s
  step '`' (a:b:st) = (Ap a b:st)
  step 's' st = S:st
  step 'k' st = K:st
  step 'i' st = I:st
  step _   st = st

nnodes :: SKI -> Int
nnodes S = 1
nnodes K = 1
nnodes I = 1
nnodes (Ap a b) = 1 + nnodes a + nnodes b

type Nbytes = Int

-- representation where every term is represented as
-- a tag (8 bytes) plus zero or more child pointers (8 bytes each).
-- tags are:
-- * S,K,I are all separate tags (0 children).
-- * Ap
-- byte count includes the pointer to the root.

adtRepr :: (String, SKI -> Int)
adtRepr = ("ADT repr", go) where
  go S = 16
  go K = 16
  go I = 16
  go (Ap a b) = 16 + go a + go b

-- variant scheme in which we have dedicated static objects
-- for S,K,I (so their pointers can be distinguished)
-- and then don't need a tag for Ap either

adtRepr_tagless :: (String, SKI -> Int)
adtRepr_tagless = ("ADT repr tagless", go) where
  go S = 8
  go K = 8
  go I = 8
  go (Ap a b) = 8 + go a + go b

-- allocate static objects for S,K,I, but
-- use a tag for variants (Ap K, etc)

adtRepr_xtracases :: (String, SKI -> Int)
adtRepr_xtracases = ("ADT repr w/ K1, S1, S2", go) where
  go S = 8
  go K = 8
  go I = 8
  go (Ap K a) = 8 + 8 + go a
  go (Ap S a) = 8 + 8 + go a
  go (Ap (Ap S a) b) = 8 + 8 + go a + go b
  go (Ap a b) = 8 + 8 + go a + go b

-- represents a packed term (consisting of tree bits and a list of child pointers)

data Tm = Tm { treebits :: Int, children :: [Tm] }

limit :: Int
limit = 64

packedRepr1 :: (String, SKI -> Int)
packedRepr1 = ("1-bit application (bottom up)", sz . build) where
  comb = Tm { treebits = 3, children = [] }
  build S = comb; build K = comb; build I = comb
  build (Ap a b) =
    case (build a, build b) of
      (t@(Tm tb1 cs1), u@(Tm tb2 cs2))
        | 1 + tb1 + tb2 <= limit -> Tm (1 + tb1 + tb2) (cs1 ++ cs2)
        | 1 + tb1 + 3   <= limit -> Tm (1 + tb1 + 3)   (cs1 ++ [u])
        | 1 + 3   + tb2 <= limit -> Tm (1 + 3   + tb2) ([t] ++ cs2)
        | otherwise              -> Tm (1 + 3   + 3)   ([t] ++ [u])
  sz (Tm tb cs) | tb <= limit = 8 + 8 + sum (sz <$> cs)

packedRepr2 :: (String, SKI -> Int)
packedRepr2 = ("1-bit application (top down)", sz . buildFull) where
  comb = Tm { treebits = 3, children = [] }
  buildFull = build limit
  build lim c
    | lim < 3 = error "can't fit anything"
    | otherwise =
      case c of
        S -> comb
        K -> comb
        I -> comb
        Ap a b ->
          if lim < 7 then {- no space to fit an application -}
            Tm 3 [buildFull c]
          else
            let t@(Tm tb1 cs1) = build (lim - 1 - 3) a
                u@(Tm tb2 cs2) = build (lim - 1 - tb1) b
            in Tm (1 + tb1 + tb2) (cs1 ++ cs2)
  sz (Tm tb cs) | tb <= limit = 8 + 8 + sum (sz <$> cs)

packedRepr3 :: (String, SKI -> Int)
packedRepr3 = ("3-bit (SKI only)", sz . build) where
  comb = Tm { treebits = 3, children = [] }
  build S = comb; build K = comb; build I = comb
  build (Ap a b) =
    case (build a, build b) of
      (t@(Tm tb1 cs1), u@(Tm tb2 cs2))
        | 3 + tb1 + tb2 <= limit -> Tm (3 + tb1 + tb2) (cs1 ++ cs2)
        | 3 + tb1 + 3   <= limit -> Tm (3 + tb1 + 3)   (cs1 ++ [u])
        | 3 + 3   + tb2 <= limit -> Tm (3 + 3   + tb2) ([t] ++ cs2) 
        | otherwise              -> Tm (3 + 3   + 3)   ([t] ++ [u])
  sz (Tm tb cs) | tb <= limit = 8 + 8 + sum (sz <$> cs)

data Tm2 = Tm2 Int [Tm2] Bool {- head is an application? -}

packedRepr4 :: (String, SKI -> Int)
packedRepr4 = ("3-bit (SKI + KS + KK + `x`yz)", sz . build) where
  comb = Tm2 3 [] False
  isApp (Tm2 _ _ a) = a
  build S = comb; build K = comb; build I = comb
  build (Ap K S) = comb
  build (Ap K K) = comb
  build (Ap a b) =
    case (build a, build b) of
      (t@(Tm2 tb1 cs1 _), u@(Tm2 tb2 cs2 _))
        | tb1 + tb2 <= limit, isApp u
                                 -> Tm2 (    tb1 + tb2) (cs1 ++ cs2) True
        | 3 + tb1 + tb2 <= limit -> Tm2 (3 + tb1 + tb2) (cs1 ++ cs2) True
        | 3 + tb1 + 3   <= limit -> Tm2 (3 + tb1 + 3)   (cs1 ++ [u]) True
        | 3 + 3   + tb2 <= limit -> Tm2 (3 + 3   + tb2) ([t] ++ cs2) True
        | otherwise              -> Tm2 (3 + 3   + 3)   ([t] ++ [u]) True
  sz (Tm2 tb cs _) | tb <= limit = 8 + 8 + sum (sz <$> cs)

data Tm3 = Tm3 Int   -- treebits
               [Tm3] -- children
               Int   -- # descendants
               Bool  -- starts with application?

packedRepr5 :: (String, SKI -> Int)
packedRepr5 = ("3-bit (SKI + KS + KK + `x`yz) + short child offsets", sz . dumpAverageTb . build) where
  comb = Tm3 3 [] 0 False
  isApp (Tm3 _ _ _ a) = a
  build S = comb; build K = comb; build I = comb
  build (Ap K S) = comb
  build (Ap K K) = comb
  build (Ap a b) =
    case (build a, build b) of
      (t@(Tm3 tb1 cs1 nd1 _), u@(Tm3 tb2 cs2 nd2 _))
        | tb1 + tb2 <= limit, isApp u
                                 -> tm3a (    tb1 + tb2) (cs1 ++ cs2)
        | 3 + tb1 + tb2 <= limit -> tm3a (3 + tb1 + tb2) (cs1 ++ cs2)
        | 3 + tb1 + 3   <= limit -> tm3a (3 + tb1 + 3)   (cs1 ++ [u])
        | 3 + 3   + tb2 <= limit -> tm3a (3 + 3   + tb2) ([t] ++ cs2)
        | otherwise              -> tm3a (3 + 3   + 3)   ([t] ++ [u])
        where tm3a tb cs = Tm3 tb cs (length cs + nd1 + nd2) True
  dumpAverageTb t = trace ("average tb per node: " ++ ratio (sum tbs') (length tbs')) t where
    tbs' = tbs t
    tbs (Tm3 tb cs _ _) = tb : concatMap tbs cs
    trace _ x = x
  nearbytes = 2
  nearlimit = 256 ^ nearbytes
  sz (Tm3 tb cs nd _) | tb <= limit =
    8 + 8 + roundup (go 0 cs) where
      roundup n = (((n - 1) `div` 8) + 1) * 8
      go csz  {- cumulative size occupied by all the children processed so far -}
         cs   {- list of children left to process -}
         = case cs of
             [] -> csz
             c:cs -> let c's_sz = sz c in
                     if csz < nearlimit then
                       go (nearbytes + c's_sz - 8 + csz) cs
                     else
                       go (nearbytes + c's_sz + csz) cs

tbl :: [[String]] -> String
tbl rows = unlines $ map mkRow rows where
  paddings = map (maximum . map length) (transpose rows)
  mkRow = intercalate "  " . zipWith pads paddings
  pads n s = s ++ replicate (n - length s) ' '

ratio :: Int -> Int -> String
ratio a b = show (fromIntegral a / fromIntegral b :: Float)

main = do
  files <- getArgs
  pgms <- mapM (fmap parse . readFile) files
  let pgm_nodecounts = map nnodes pgms
  let pgm_totalnodecount = sum pgm_nodecounts
  let hdr = ["pgm"] ++ files ++ ["wavg"]
  let reprs = [ adtRepr
              , adtRepr_tagless
              , adtRepr_xtracases
              , packedRepr1
              , packedRepr2
              , packedRepr3
              , packedRepr4
              , packedRepr5
              ]
  let cols = map (\(nm, rf) ->
                    let bytecounts = map rf pgms in
                    nm : zipWith ratio bytecounts pgm_nodecounts
                       ++ [ratio (sum bytecounts) pgm_totalnodecount])
                 reprs
  putStr $ tbl (hdr : cols)
