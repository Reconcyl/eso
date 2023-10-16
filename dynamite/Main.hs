{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main (main) where

import Prelude hiding (fail)

import System.IO (hPutStrLn, stderr)
import System.Environment (getArgs)
import qualified Control.Monad.Reader as Reader
import Control.Monad.Reader (ReaderT)
import Data.Char (isDigit, isSpace)
import Data.Map.Strict (Map)
import qualified Data.Map as Map
import Data.Proxy (Proxy (Proxy))

newtype Sym = Sym { unSym :: String } deriving (Eq, Ord)

data Val = VInt Integer
         | VStr String
         | VSym Sym
         | VList [Val]
         | VFun ([Form] -> Dynamite Val)

tag :: Val -> String
tag (VInt  _) = "integer"
tag (VStr  _) = "string"
tag (VSym  _) = "symbol"
tag (VList _) = "list"
tag (VFun  _) = "function"

instance Show Val where
  show (VInt i)   = show i
  show (VStr s)   = show s
  show (VSym s)   = "'" ++ unSym s
  show (VList xs) = "(" ++ unwords (show <$> xs) ++ ")"
  show (VFun f)   = "#<function>"

parse :: String -> Either String (Maybe Form, String)
parse (c:cs)     | isSpace c = parse cs
parse (cs@(c:_)) | isDigit c = let (digits, cs') = span isDigit cs  in Right (Just (VInt (read digits)), cs')
parse ('"':cs)               = let (st, '"':cs') = span (/= ':') cs in Right (Just (VStr st),            cs') -- TODO: escapes
parse (')':cs)               =                                         Right (Nothing,                   cs)
parse ('(':cs)               = go [] cs where
  go acc cs = parse cs >>= \case
    (Nothing, cs') -> Right (Just (VList (reverse acc)), cs')
    (Just v,  cs') -> go (v : acc) cs'
parse cs@(_:_)               = let (s,      cs') = span ident cs    in Right (Just (VSym (Sym s)),       cs')
                                 where ident c = not (isSpace c || c == ')')
parse []                     = Left "unexpected EOF"

parse1 s = parse s >>= \case
  (Nothing, _) -> Left "unexpected )"
  (Just v, cs) -> case dropWhile isSpace cs of
                    []  -> Right v
                    c:_ -> Left ("unexpected trailing character: " ++ show c)

type Form = Val

newtype Dynamite a = Dynamite { runDynamite :: ReaderT Env (Either String) a
                              } deriving (Functor, Applicative, Monad)
deriving instance (Reader.MonadReader Env Dynamite)

type Env = Map Sym Val

fail :: String -> Dynamite a
fail = Dynamite . Reader.lift . Left

eval :: Form -> Dynamite Val
eval (VSym s) =
  Reader.asks (Map.lookup s) >>= \case
    Just v  -> pure v
    Nothing -> fail $ "unbound variable: " ++ unSym s
eval (VList (ff : args)) =
  eval ff >>= \case
    VFun f -> f args
    _      -> fail $ "not a function: " ++ show ff
eval v = pure v

class Arg a where
  fromForm :: Form -> Dynamite a

instance Arg Val where
  fromForm f = pure f

instance Arg Integer where
  fromForm (VInt n) = pure n
  fromForm v        = fail $ "expected integer, got " ++ tag v

instance Arg Sym where
  fromForm (VSym s) = pure s
  fromForm v        = fail $ "expected symbol, got " ++ tag v

newtype Eval a = Eval a
instance Arg a => Arg (Eval a) where
  fromForm v = do v' <- eval v
                  fmap Eval $ fromForm v'

class Erase a where
  erase :: (Int, a -> [Form] -> Dynamite Val)

instance Erase Val where
  erase = (0, \v [] -> pure v)

instance Erase (Dynamite Val) where
  erase = (0, \act [] -> act)

instance (Arg a, Erase e) => Erase (a -> e) where
  erase = (n+1, \act (a:as) -> do a' <- fromForm a
                                  f (act a') as
          ) where (n, f) = erase

fn :: Erase e => String -> e -> (Sym, Val)
fn name body = (Sym name,
                VFun $ \args -> let m = length args in
                                if n == m then f body args
                                          else fail $ name ++ " expected " ++ show n ++ " arguments, got " ++ show m
               ) where (n, f) = erase

initialEnv :: Env
initialEnv = Map.fromList
  [ fn "+"    $ \(Eval n1) (Eval n2) -> VInt (n1 + n2)
  , fn "eval" $ \(Eval (Eval x))     -> x :: Val
  , fn "let"  $ \sym (Eval v1) e2    -> Reader.local (Map.insert sym v1) (eval e2)
  -- TODO: make this more primitive and implement fun in userspace
  , fn "fun"  $ \sym body            -> VFun $ \args -> Reader.local (Map.insert sym (VList args)) (eval body)
  ]

main :: IO ()
main = do
  let go acc []     = pure $ Right $ reverse acc
      go acc (f:fs) = parse1 <$> readFile f >>= \case
                        Right v -> go (v : acc) fs
                        Left e -> pure $ Left $ "can't parse file " ++ show f ++ ": " ++ e
  getArgs >>= go [] >>= \case
    Left e -> hPutStrLn stderr e
    Right forms -> print $ Reader.runReaderT (runDynamite (eval (VList forms))) initialEnv
