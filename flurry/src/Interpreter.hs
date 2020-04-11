{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}

module Interpreter (run) where

import Control.Monad (ap)
import Data.Maybe (catMaybes)

import AST (AST, Ins (..))


-- In theory, only the first constructor is needed. The other three are
-- used to determine whether a term represents a Church numeral.
data Expr
    = Func (Expr -> Flurry Expr)
    | ChurchZTest Int
    | ChurchFTest
    | Error

type Stack = [Expr]

-- Apply one expression to another. Errors will tend to propagate
-- unless passed to a function that ignores its argument.
runExpr :: Expr -> Expr -> Flurry Expr
runExpr (Func f)    x               = f x
runExpr ChurchFTest (ChurchZTest n) = pure $ ChurchZTest $ n + 1
runExpr _           _               = pure Error


-- Define a state monad for the stack.

newtype Flurry a = Flurry { runFlurry :: Stack -> (a, Stack) } deriving (Functor)

instance Applicative Flurry where
    pure a = Flurry $ \s -> (a, s)
    (<*>) = ap

instance Monad Flurry where
    return = pure
    fa >>= f = Flurry $ \s -> let (a, s') = runFlurry fa s in runFlurry (f a) s'


-- Stack manipulation operations

push :: Expr -> Flurry ()
push e = Flurry $ \s -> ((), e : s)

pop :: Flurry Expr
pop = Flurry $ \case
    (e : es) -> (e, es)
    []       -> (combI, []) -- return the I combinator if the stack is empty


-- Combinators

combI :: Expr
combI = Func pure

combK :: Expr
combK = Func $ \a -> pure $ Func $ \_ -> pure a

combS :: Expr
combS = Func $ \a -> pure $ Func $ \b -> pure $ Func $ \c -> do
    ac <- runExpr a c
    bc <- runExpr b c
    runExpr ac bc

church :: Int -> Expr
church n = Func $ \f -> pure $ Func $ \z -> repM n f z
    where
        repM n' f z
            | n' <  0 = error "negative numbers cannot be made into church numerals"
            | n' == 0 = pure z
            | n' >  0 = do
                prev <- repM (n' - 1) f z
                runExpr f prev


-- Main interpreter logic

type Op = Flurry Expr

-- Combine a list of operators into a single one using a function that describes
-- how their return values should be combined.
sequenceOp :: (Expr -> Expr -> Op) -> Op -> [Op] -> Op
sequenceOp _ acc []     = acc
sequenceOp f acc (x:xs) = sequenceOp f combined xs
    where
        combined = do
            acc' <- acc
            x' <- x
            f acc' x'

reduceApp, reduceComp :: Op -> [Op] -> Op
reduceApp  = sequenceOp runExpr
reduceComp = sequenceOp $ \f g -> pure $ Func $ \x -> do
    gx <- runExpr g x
    runExpr f gx

eval :: Ins -> Op

eval (Paren  [])     = pure combK
eval (Paren  (f:xs)) = do
    res <- reduceApp (eval f) (map eval xs)
    push res
    pure res

eval (Square [])     = Flurry $ \s -> (church (length s), s)
eval (Square (f:xs)) = reduceApp (eval f) (map eval xs)

eval (Curly  [])     = pop
eval (Curly  (f:xs)) = pure $ Func $ \x -> do
    push x
    reduceApp (eval f) (map eval xs)

eval (Angle  [])     = pure combS
eval (Angle  (f:xs)) = reduceComp (eval f) (map eval xs)

-- Attempt to convert a Church numeral back into its numeric representation.
-- The numeral is executed with an empty stack and it is considered an invalid
-- numeral if it leaves the stack nonempty.
dechurch :: Expr -> Maybe Int
dechurch f =
    case runFlurry evalOnFZ [] of
        (ChurchZTest n, []) -> Just n
        _                   -> Nothing
    where
        evalOnFZ = do
            ff <- runExpr f ChurchFTest
            fz <- runExpr ff $ ChurchZTest 0
            return fz

run :: AST -> [Int] -> (Maybe Int, [Int])
run ast inputs = (returnVal, outputs)
    where
        initStack = reverse $ map church inputs
        ops = map eval ast
        combined = reduceApp (pure combI) ops
        (returnExpr, finalStack) = runFlurry combined initStack
        returnVal = dechurch returnExpr
        outputs = reverse $ catMaybes $ map dechurch finalStack
