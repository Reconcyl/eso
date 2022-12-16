{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}

module Parser (Ins' (..), Ins, Fn (..), Pgm (..), parsePgm) where

import Data.Either (partitionEithers)
import Data.List (group, sort, intercalate)
import Data.Map (Map)
import qualified Data.Map as Map

import Text.Parsec (Parsec)
import qualified Text.Parsec as Parsec

data Ins' s f
  = Add0 | Add1
  | Call s | Ret | RestartCaller | Recur
  | Anon f | Cond (Ins' s f)
  | Block [Ins' s f]
  | Input | Output
deriving instance (Show s, Show f) => Show (Ins' s f)
deriving instance (Eq s, Eq f) => Eq (Ins' s f)

type Ins = Ins' String Fn
newtype Fn = Fn Ins deriving (Show)

data Pgm = Pgm {
    fns :: Map String Ins,
    main_ :: [Ins]
  }
  deriving (Show)

parsePgm :: String -> Either String Pgm
parsePgm s =
  let s' = clean s in
    case Parsec.parse pgm {- source file name: -} "" s' of
      Left e -> Left (show e)
      Right toplevel ->
        let (fns, main) = partitionEithers toplevel in
        case duplicates (fst <$> fns) of
          [] -> Right $ Pgm (Map.fromList fns) main
          [dup] -> Left $ "function " ++ dup ++
                          " has multiple definitions"
          dups -> Left $ "functions " ++ intercalate ", " dups ++
                         "have multiple definitions"
  where
    clean :: String -> String
    clean (' ':s) = clean s
    clean ('\t':s) = clean s
    clean ('\r':s) = clean s
    clean ('\n':s) = clean s
    clean (';':s) = clean (dropWhile (/= '\n') s)
    clean (c:s) = c : clean s
    clean [] = []

    duplicates :: Ord a => [a] -> [a]
    duplicates xs = [head g | g <- group (sort xs), length g > 1]

    (<|>) = (Parsec.<|>); (<?>) = flip (Parsec.<?>); many = Parsec.many
    char = Parsec.char; oneOf = Parsec.oneOf; noneOf = Parsec.noneOf

    pgm :: Parsec String () [Either (String, Ins) Ins]
    pgm = many toplevel <* Parsec.eof

    toplevel :: Parsec String () (Either (String, Ins) Ins)
    toplevel = (Left <$> fndef) <|> (Right <$> instruction)

    ident = "identifier" <?> do
      let alpha = ['a'..'z'] ++ ['A'..'Z'] ++ "_"
      let alnum = ['0'..'9'] ++ alpha
      h <- oneOf alpha; t <- many (oneOf alnum); pure (h:t)

    fndef :: Parsec String () (String, Ins)
    fndef = "function definition" <?> do
      char ':'; n <- ident; b <- instruction; pure (n, b)

    instruction :: Parsec String () Ins
    instruction = "instruction" <?> do
      noneOf ")" >>= \case
        '0' -> pure Add0
        '1' -> pure Add1
        '>' -> Call <$> ident
        '<' -> pure Ret
        '^' -> pure RestartCaller
        '"' -> pure Recur
        '\'' -> Anon . Fn <$> instruction
        '?' -> Cond <$> instruction
        '(' -> Block <$> many instruction <* char ')'
        ',' -> pure Input
        '.' -> pure Output
        c -> fail $ "invalid command " ++ show c
