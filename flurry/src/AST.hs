{-# LANGUAGE LambdaCase #-}

module AST (Ins (..), AST, parse) where

-- An empty AST parameter indicates a nilad.
data Ins
    = Paren  AST
    | Square AST
    | Curly  AST
    | Angle  AST
    deriving (Show)

type AST = [Ins]

mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f (a, b) = (f a, b)

parseParenthesized :: (AST -> Ins) -> Char -> String -> Maybe (AST, String)
parseParenthesized makeIns endChar s
    = parse' s >>= \case
        (inner, (c : s')) | c == endChar
            -> mapFst (makeIns inner :) <$> parse' s'
        _   -> Nothing

parse' :: String -> Maybe (AST, String)
parse' ('(':s) = parseParenthesized Paren ')' s
parse' ('[':s) = parseParenthesized Square ']' s
parse' ('{':s) = parseParenthesized Curly '}' s
parse' ('<':s) = parseParenthesized Angle '>' s
parse' (c:s)
    | c `elem` ")]}>" = Just ([], c:s)
    | otherwise       = parse' s
parse' ""      = Just ([], "")

parse :: String -> Maybe AST
parse s = parse' s >>= \case
    (ast, "") -> Just ast
    _         -> Nothing
