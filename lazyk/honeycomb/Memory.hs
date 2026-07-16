{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}

import Data.List (transpose, intercalate)
import Data.Maybe (fromJust, fromMaybe)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Control.Monad (guard)
import Control.Monad.ST (ST)
import qualified Control.Monad.ST as ST
import qualified Data.STRef as ST

import Text.Printf (printf)
import System.Environment (getArgs)
import Debug.Trace

import Control.Comonad.Cofree (Cofree ((:<)))
import Data.Functor.Foldable (Base, Recursive (project), histo)

data SKI = S | K | I | Ap SKI SKI deriving (Show)

data SKIF a = SF | KF | IF | ApF a a deriving (Functor)
type instance (Base SKI) = SKIF
instance Recursive SKI where
  project S = SF
  project K = KF
  project I = IF
  project (Ap x y) = ApF x y

memoizeTermRecursion ::
  Ord k =>
  (forall id. (Cofree SKIF id -> k -> ST s b)
           -> (Cofree SKIF id -> k -> ST s b)) ->
  ST s (SKI -> k -> ST s b)
memoizeTermRecursion f = do
  nextID <- ST.newSTRef 3
  -- annotate a term with distinct integers for all distinct terms
  let annotate S        = pure (0 :< SF)
      annotate K        = pure (1 :< KF)
      annotate I        = pure (2 :< IF)
      annotate (Ap a b) = do aid <- ST.readSTRef nextID
                             ST.modifySTRef' nextID (+ 1)
                             a' <- annotate a
                             b' <- annotate b
                             pure (aid :< ApF a' b')
  -- memoization cache
  cache <- ST.newSTRef Map.empty
  let go tm@(id :< _) k = do cached <- Map.lookup (id, k) <$> ST.readSTRef cache
                             case cached of
                               Just b -> pure b
                               Nothing -> do b <- f go tm k
                                             ST.modifySTRef' cache (Map.insert (id, k) b)
                                             pure b
  pure (\tm k -> do
    tm' <- annotate tm
    go tm' k)

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
packedRepr4 = ("3-bit (SKI, KS/KK, `x`yz)", sz . build) where
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
packedRepr5 = ("3-bit (SKI, KS/KK, `x`yz, short child offsets)", sz . dumpAverageTb . build) where
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

-- a new scheme for packing a term into a limited number of bits.
-- tags are packed into the low bits:
--  000  S
--  010  K
--  100  I
--   01  application (remaining bits divided evenly - function low,
--         argument high, bias toward argument when available space is odd)
--   11  escape to the child term list
--
-- unused space is padded with 0s.
--
-- also, it is illegal for the packing to be done in such a way that
-- two consecutive 1s are introduced except as part of the escape sequence.
-- (if this happens, the term on the right needs to be padded with
-- an additional 0.

data WideTm = WideTm String   -- representation
                     [WideTm] -- children

(<?>) :: Maybe WideTm -> Maybe WideTm -> Maybe WideTm
m1 <?> m2 = do
  WideTm r1 c1 <- m1
  WideTm r2 c2 <- m2
  guard (not (last r1 == '1' && head r2 == '1'))
  pure (WideTm (r1 <> r2) (c1 <> c2))

atom r = Just (WideTm r [])
pad sz (WideTm r c) | n > sz    = Nothing
                    | otherwise = Just (WideTm r' c)
                    where n = length r
                          r' = replicate (sz - n) '0' ++ r

layout :: (Cofree SKIF id -> Int -> ST s (Maybe WideTm)) ->
           Cofree SKIF id -> Int -> ST s (Maybe WideTm)
-- layout rec width

{-
build :: SKI -> (Maybe WideTm, Int -> Maybe WideTm)
build = histo $ \case
  SF -> (Nothing, \sz -> pad sz =<< atom "")
  KF -> (Nothing, \sz -> pad sz =<< atom "10")
  IF -> (Nothing, \sz -> pad sz =<< atom "100")
  ApF ((rootA, layoutA) :< a) ((rootB, layoutB) :< b) ->
    let
      root = <?>
    -- rootA, rootB     :: Maybe WideTM
    -- layoutA, layoutB :: Int -> Maybe WideTm
    -- a, b             :: SKIF (Cofree SKIF (Maybe WideTM, Int -> Maybe WideTm))
    -- ---------------------------------------------------------
    -- goal :: (Maybe WideTm, Int -> Maybe WideTm)
    _
-}

wideRepr1 :: (String, SKI -> Int)
wideRepr1 = ("wideRepr1 (SKI only, top down)", sz . buildRoot) where
  -- atom r = Just (WideTm r [])
  buildRoot = fromJust . build limit
  build _ _ = undefined
  {-
  build sz S = pad sz <$>                     atom ""
  build sz K = pad sz <$> do guard (sz >= 2); atom "10"
  build sz I = pad sz <$> do guard (sz >= 3); atom "100"
  build sz (Ap a b) = (buildAp (sz - 2) (True, a) (True, a)
                   <|> buildAp (sz - 2) (True, a) (False, a)
                   <|> buildAp (sz - 2) (False, a) (True, a)
                   <|> buildAp (sz - 2) (False, a) (False, a)) <> atom "01"
  buildAp :: Int -> (Bool, SKI) -> (Bool, SKI) -> Maybe WideTm
  buildAp sz ma mb = undefined
  mbuild (True, a) = build a
  -- mbuild (False, a) =
  -}
  
  sz :: WideTm -> Int
  sz = undefined

tbl :: [[String]] -> String
tbl rows = unlines $ map mkRow rows where
  paddings = map (maximum . map length) (transpose rows)
  mkRow = intercalate "  " . zipWith pads paddings
  pads n s = s ++ replicate (n - length s) ' '

-- generate a spine for xs that doesn't force its computation, assuming its length is n
withLen :: Int -> [a] -> [a]
withLen n _ | n <= 0 = []
withLen n ~(x : xs)  = x : withLen (n - 1) xs

ratio :: Int -> Int -> String
ratio a b = withLen 8 $ printf "%8.4f" (fromIntegral a / fromIntegral b :: Float)

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
              , wideRepr1
              ]
  let cols = map (\(nm, rf) ->
                    let bytecounts = map rf pgms in
                    nm : zipWith ratio bytecounts pgm_nodecounts
                       ++ [ratio (sum bytecounts) pgm_totalnodecount])
                 reprs
  putStr $ tbl $
    (if length files == 1 then map init else id)
    (hdr : cols)
