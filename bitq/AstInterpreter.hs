{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}

module Main (main) where

import qualified Data.Bits
import Data.List (group, sort, intercalate)
import Data.Either (partitionEithers)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Foldable (toList)

import System.IO (isEOF, stdin, stdout, stderr,
                  hPutStr, hSetBinaryMode, readFile)
import System.Environment (getArgs, getProgName)

import Text.Parsec (Parsec)
import qualified Text.Parsec as Parsec

data Ins
  = Add0 | Add1
  | Call String | Ret | RestartCaller | Recur
  | Anon Ins | Cond Ins
  | Block [Ins]
  | Input | Output
  deriving (Show)

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
        '\'' -> Anon <$> instruction
        '?' -> Cond <$> instruction
        '(' -> Block <$> many instruction <* char ')'
        ',' -> pure Input
        '.' -> pure Output
        c -> fail $ "invalid command " ++ show c

data Bit = B0 | B1
instance Show Bit where
  show B0 = "0"; show B1 = "1"
  showList = (++) . map (\case B0 -> '0'; B1 -> '1')

bit :: Int -> Bit
bit 0 = B0; bit 1 = B1; bit _ = error "invalid bit"

unbit :: Bit -> Int
unbit B0 = 0; unbit B1 = 1

data Execution = Execution {
    frames :: [(Ins, [Ins])],
    queue :: Seq Bit
  }

initEx :: [Ins] -> Execution
initEx ins = Execution
  { frames = [(Block ins, ins)]
  , queue = Seq.Empty }

instance Show Execution where
  show (Execution frames queue) = unlines ls ++ "\x1b[39m" where
    ls :: [String]
    ls = "\n\x1b[94m"
       : ("> " ++ show (toList queue))
       : ["| " ++ (frame >>= showIns "") | (_, frame) <- frames]
    showIns :: String -> Ins -> String
    showIns acc = \case
      Add0 -> '0':acc
      Add1 -> '1':acc
      Call fn -> '>' : fn ++ acc
      Ret -> '<':acc
      RestartCaller -> '^':acc
      Recur -> '"':acc
      Anon f' -> '\'' : showIns acc f'
      Cond c -> '?' : showIns acc c
      Block b -> '(' : foldr (flip showIns) (')' : acc) b
      Input -> ',':acc
      Output -> '.':acc

step :: Map String Ins -> Execution -> Maybe (IO Execution)
step fns (Execution { frames, queue }) =
  case frames of
    [] -> Nothing
    (_,[]):frames' -> Just $ pure $ Execution frames' queue
    (f,(ins:frame')):frames' ->
      let step f q = Just $ pure $ Execution f q in
      case ins of
        Add0 -> step ((f,frame'):frames') (queue Seq.|> B0)
        Add1 -> step ((f,frame'):frames') (queue Seq.|> B1)
        Call fn ->
          case Map.lookup fn fns of
            Nothing -> fail $ "no such function: " ++ fn
            Just body -> step ((body,[body]):(f,frame'):frames') queue
        Ret -> step frames' queue
        RestartCaller ->
          case frames' of
            (f,_):frames'' -> step ((f,[f]):frames'') queue
            [] -> fail "can't restart at top level"
        Recur -> step ((f,[f]):(f,frame'):frames') queue
        Anon f' -> step ((f',[f']):(f,frame'):frames') queue
        Cond c ->
          case queue of
            bit Seq.:<| queue' ->
              let pref = (case bit of B0 -> []; B1 -> [c]) in
              step ((f,pref++frame'):frames') queue'
            Seq.Empty -> Nothing
        Block b -> step ((f, b ++ frame'):frames') queue
        Input -> Just $ isEOF >>= \case
          True -> pure $ Execution ((f,frame'):frames') queue
          False -> do
            c <- fromEnum <$> getChar
            let (.&.) = (Data.Bits..&.); (>>) = Data.Bits.shiftR
            let bits =
                  [ bit $ (c >> shift) .&. 1
                  | shift <- reverse [0..7] ]
            let queue' = queue <> Seq.fromList bits
            pure $ Execution ((f,frame'):frames') queue
        Output ->
          if Seq.length queue < 8 then Nothing
          else
            let (bits, queue') = Seq.splitAt 8 queue in
            let c = foldl (\a b -> a * 2 + unbit b) 0 bits in
            Just $ do putChar (toEnum c)
                      pure (Execution ((f,frame'):frames') queue')

help :: String -> String
help prog = unlines
  ["Usage:",
   prog ++ " <file>: run program stored in file",
   prog ++ " debug <file>: run program in debug mode"]

getProgram :: FilePath -> IO Pgm
getProgram f = do
  code <- readFile f
  case parsePgm code of
    Left e -> fail $ "can't parse file " ++ f ++ ": " ++ e
    Right pgm -> pure pgm

main :: IO ()
main = getArgs >>= \case
  [f] -> do
    Pgm { fns, main_ } <- getProgram f
    hSetBinaryMode stdin True
    hSetBinaryMode stdout True
    let go = maybe (return ()) (>>= go) . step fns
    go (initEx main_)
    hSetBinaryMode stdin False
    hSetBinaryMode stdout False
  ["debug", f] -> do
    hSetBinaryMode stdout True
    Pgm { fns, main_ } <- getProgram f
    let go ex = do
          hPutStr stderr $ show ex
          case step fns ex of
            Nothing -> return ()
            Just f -> do
              wait <- getChar
              hSetBinaryMode stdin True
              ex' <- f
              hSetBinaryMode stdin False
              go ex'
    go (initEx main_)
    hSetBinaryMode stdout False
  _ -> getProgName >>= putStr . help