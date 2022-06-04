{-# LANGUAGE LambdaCase #-}

-- Brute-force search tool used to find small terms with certain properties.
-- In particular, this was used to search for terms `t` where `t I ≠ I` but
-- `t (t I) = I` or `t (t (t I)) = I`, which are used to check for a Church
-- numeral's remainder mod 2 and 3 respectively.

import Control.Applicative (liftA2)
import Control.Monad (guard)
import Data.Maybe (fromMaybe, isJust)

-- proportion of elements for which `p` holds
ratio :: (a -> Bool) -> [a] -> Double
ratio p xs = len (filter p xs) / len xs where len = fromIntegral . length

data Term = S | K | I | V Int | Term :@ Term deriving (Show)

-- return a list whose nth element is f applied to the first n elements in reverse order
iterate' :: ([a] -> a) -> [a]
iterate' f = go [] where go pref = f pref : go (f pref : pref)

isValid :: Term -> Bool
-- variables should not be treated as functions
isValid (V _ :@ _) = False
-- redundant patterns
isValid (I :@ _) = False
isValid (K :@ _ :@ _) = False
isValid (S :@ K) = False
-- no variable should be used twice
isValid t = isJust (go t) where
  go :: Term -> Maybe (Bool, Bool, Bool)
  go (V 0) = Just (True, False, False)
  go (V 1) = Just (False, True, False)
  go (V 2) = Just (False, False, True)
  go (a :@ b) = do
    (v0a,v1a,v2a) <- go a
    (v0b,v1b,v2b) <- go b
    guard $ not $ v0a && v0b || v1a && v1b || v2a && v2b
    return       (v0a || v0b,   v1a || v1b,   v2a || v2b)
  go _ = Just (False, False, False)

-- the nth element is all valid terms of size `n+1` from the provided alphabet
mkTerms :: [Term] -> [[Term]]
mkTerms alpha = iterate' $ \case [] -> alpha; xs -> filter isValid $ concat $ zipWith (liftA2 (:@)) xs (reverse xs)
terms = mkTerms [S,K,I]
terms3 = mkTerms [S,K,I,V 0,V 1,V 2]

-- perform 1 step reduction or return Nothing if no reduction is possible
reduce :: Term -> Maybe Term
reduce (I :@ a) = Just a
reduce (S :@ K :@ _) = Just I
reduce (K :@ x :@ _) = Just x
reduce (S :@ x :@ y :@ z) = Just $ (x :@ z) :@ (y :@ z)
reduce (a :@ b) =
  case (reduce a, reduce b) of
    (Just a, Nothing) -> Just $ a :@ b
    (Nothing, Just a) -> Just $ a :@ b
    (Just a,  Just b) -> Just $ a :@ b
    (Nothing, Nothing) -> Nothing
reduce _ = Nothing

-- simplify until done or return Nothing at max steps
simplify :: Term -> Maybe Term
simplify = go 20 where
  go :: Int -> Term -> Maybe Term
  go 0 t = Nothing
  go n t = case reduce t of Nothing -> Just t; Just t' -> go (n-1) t'

-- is this the identity function?
isI :: Term -> Bool
isI I = True; isI _ = False

-- do the iterates at this term at I have period 3?
isSucc3 :: Term -> Bool
isSucc3 t = fromMaybe False $ do
  t <- simplify t
  t1 <- simplify (t :@ I)
  guard $ not (isI t1)
  t2 <- simplify (t :@ t1)
  t3 <- simplify (t :@ t2)
  return (isI t3)

-- does this term distinguish S, K, and I? (search `terms3`)
isWeird :: Term -> Bool
isWeird t = fromMaybe False $ do
  V 0 <- simplify (t :@ S)
  V 1 <- simplify (t :@ K)
  V 2 <- simplify (t :@ I)
  return True

-- determine if a term becomes smaller when simplified
shrinks :: Term -> Bool
shrinks t = fromMaybe False $ do
  t' <- simplify t
  return $ size t' < size t
  where
    size :: Term -> Int
    size (a :@ b) = size a + size b
    size _ = 1

-- how many terms in the search set can shrink?
shrinkingFraction :: IO ()
shrinkingFraction = print $ ratio shrinks $ concat $ take 9 terms

main :: IO ()
-- main = print $ take 4 $ filter isSucc3 $ concat $ take 6 terms3
main = let n = 8 in print (n, length $ concat $ take n terms3)
