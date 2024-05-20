{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Control.Monad.State (State, state, evalState)

import Data.Functor ((<&>))
import Data.Function (fix)
import System.Environment (getArgs)

data SKI = S | K | I | Ap SKI SKI deriving (Show)

fromSingleton :: [a] -> a
fromSingleton [x] = x
fromSingleton _ = error "not a singleton"

parse :: String -> SKI
parse = fromSingleton . foldr step [] where
  step '`' (a:b:st) = (Ap a b:st)
  step 's' st = S:st
  step 'k' st = K:st
  step 'i' st = I:st
  step _   st = st

data Value = Neutral Neutral | Lambda (Value -> Value)
data Neutral = Var String | Neutral :@ Value

infixl :@

apply :: Value -> Value -> Value
apply (Neutral n) v = Neutral (n :@ v)
apply (Lambda f)  v = f v

class Apply a where (@@) :: Value -> a -> Value
infixl @@

instance Apply Value   where v @@ n = apply v n
instance Apply String  where v @@ s = apply v (Neutral (Var s))

pair :: Value -> Value -> Value
pair x y = Lambda $ \p -> p @@ x @@ y

num :: Int -> Value
num n = Lambda $ \f -> Lambda $ \x -> let go 0 = x
                                          go k = f @@ go (k - 1)
                                      in go n

-- monad where we have access to a supply of fresh names
type Fresh = State [String]

fresh :: Fresh String
fresh = state (\case v:vs -> (v, vs)
                     []   -> error "ran out of names")

withSupply :: String -> Fresh a -> a
withSupply prefix f = evalState f [prefix ++ show n | n :: Int <- [1..]]

sem :: SKI -> Value
sem I        = Lambda $ \x -> x
sem K        = Lambda $ \x -> Lambda $ \_ -> x
sem S        = Lambda $ \x -> Lambda $ \y -> Lambda $ \z -> (x @@ z) @@ (y @@ z)
sem (Ap t u) = sem t @@ sem u

neutral :: Value -> Fresh (Maybe Neutral)
neutral (Neutral n) = pure (Just n)
neutral (Lambda f)  = do
  -- this looks like a lambda, but maybe it is a neutral term,
  -- and we just need to eta reduce
  v <- fresh
  neutral (f (Neutral (Var v))) >>= \case
    Just (n :@ arg) -> neutral arg <&> \case Just (Var v') | v == v' -> Just n
                                             _                       -> Nothing
    _               -> return Nothing

numNeutral :: String -> String -> Int -> Value -> Fresh (Maybe Int)
numNeutral f x n v = neutral v >>= \case
  Just (Var x')       | x' == x -> pure (Just n)
  Just (Var f' :@ t') | f' == f -> numNeutral f x (n + 1) t'
  _                             -> pure Nothing

fromStream :: Value -> String
fromStream v =
  case withSupply "a" $ neutral (v @@ "p") of
    Just (Var "p" :@ x :@ y) ->
      case withSupply "b" $ numNeutral "f" "x" 0 (x @@ "f" @@ "x") of
        Just n | n >= 256  -> ""
               | otherwise -> toEnum n : fromStream y
        Nothing            -> error "stream head is not a number"
    _ -> error "stream is not a pair"

toStream :: String -> Value
toStream = foldr pair eofs . map (num . fromEnum)
  where eofs = fix (pair (num 256))

main :: IO ()
main = do
  [file] <- getArgs
  pgm <- sem . parse <$> readFile file
  interact (fromStream . apply pgm . toStream)
