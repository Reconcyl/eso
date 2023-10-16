{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE LambdaCase #-}

import Prelude hiding (fail)

import System.IO (hPutStrLn, stderr)
import System.Environment (getArgs)
import Data.Char (isDigit, isSpace)
import Data.Map.Strict (Map)
import qualified Data.Map as Map

data Val = VInt Integer
         | VStr String
         | VSym String
         | VList [Val]
         | VFun (Env -> [Form] -> Dynamite Val)

instance Show Val where
  show (VInt i)   = show i
  show (VStr s)   = show s
  show (VSym s)   = "'" ++ s
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
parse cs@(_:_)               = let (s,      cs') = span ident cs    in Right (Just (VSym s),             cs')
                                 where ident c = not (isSpace c || c == ')')
parse []                     = Left "unexpected EOF"

parse1 s = parse s >>= \case
  (Nothing, _) -> Left "unexpected )"
  (Just v, cs) -> case dropWhile isSpace cs of
                    []  -> Right v
                    c:_ -> Left ("unexpected trailing character: " ++ show c)

type Form = Val

type Dynamite = Either String
type Env = Map String Val

fail :: String -> Dynamite a
fail = Left

eval :: Env -> Form -> Dynamite Val
eval env (VSym s) =
  case Map.lookup s env of
    Just v  -> pure v
    Nothing -> fail $ "unbound variable: " ++ s
eval env (VList (ff : args)) =
  eval env ff >>= \case
    VFun f -> f env args
    _      -> fail $ "not a function: " ++ show ff
eval env v = pure v

initialEnv :: Env
initialEnv = Map.fromList
  [ ("+", VFun $ \_ args -> case args of [VInt n1, VInt n2] -> pure (VInt (n1 + n2))
                                         _                  -> fail "invalid arguments")
  , ("eval",
          VFun $ \env args -> case args of [e] -> eval env e
                                           _   -> fail "eval takes 1 argument")
  ]

main :: IO ()
main = do
  let go acc []     = pure $ Right $ reverse acc
      go acc (f:fs) = parse1 <$> readFile f >>= \case
                        Right v -> go (v : acc) fs
                        Left e -> pure $ Left $ "can't parse file " ++ show f ++ ": " ++ e
  getArgs >>= go [] >>= \case
    Left e -> hPutStrLn stderr e
    Right forms -> print $ eval initialEnv (VList forms)
