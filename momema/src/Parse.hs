module Parse (ParseMode (..), parse) where

import Control.Monad.Trans.State.Lazy (State (..))
import qualified Control.Monad.Trans.State.Lazy as State

import Control.Monad.Trans.Except (ExceptT (..))
import qualified Control.Monad.Trans.Except as Except

import Control.Monad.Trans.Class (lift)

-- Represent the command list as a vector.
import qualified Data.Vector as Vector
import Data.Vector (Vector)

-- Import the Expression and Instruction types from Base.
import Base (Expression (..), Instruction (..), Program)

type ErrorMsg = String

-- A program unit paired with a row and column number.
type Located a = (Int, Int, a)

digit = "1234567890"
lowercase = "abcdefghijklmnopqrstuvwxyz"
uppercase = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

genError :: Located String -> String
genError (line, col, message) = concat ["Line ", show line, ", column ", show col, ": ", message]

data ParseMode = NormalMode
               | DebugMode
               | InteractiveMode
               deriving (Eq, Ord)

data ParseState = ParseState {
    getParseMode :: ParseMode,
    getLineNo :: Int,
    getColNo :: Int,
    getRest :: String
}
type Parser = State ParseState
type ErrParser = ExceptT (Located ErrorMsg) Parser

-- Return the next character in the parse stream, or Nothing if the parse stream is empty.
peek :: Parser (Maybe Char)
peek = do
    state <- State.get
    return $ case getRest state of
        [] -> Nothing
        x:_ -> Just x

-- Return and delete the next character in the parse stream, or Nothing if the parse stream is
-- empty. Update `getLineNo` and `getColNo` as necessary to reflect the actual position in the
-- source file.
pop :: Parser (Maybe Char)
pop = do
    state <- State.get
    case getRest state of
        [] -> return Nothing
        c:rest -> do
            if c == '\n' then do
                state <- State.get
                State.put $ state { getLineNo = getLineNo state + 1, getColNo = 0 }
            else do
                state <- State.get
                State.put $ state { getColNo = getColNo state + 1 }
            state <- State.get
            State.put $ state { getRest = rest }
            return $ Just c

-- Take a list of characters representing an alphabet, and keep popping characters as long as they
-- belong to that alphabet. Return every character popped.
parseAlphabet :: String -> Parser String
parseAlphabet alphabet = reverse <$> parseAlphabet' alphabet []
    where
        parseAlphabet' alphabet acc = do
            char <- peek
            case char of
                Nothing -> return acc
                Just c
                    | c `elem` alphabet -> pop >> parseAlphabet' alphabet (c:acc)
                    | otherwise -> return acc

-- Like `peek`, but keep popping until a non-whitespace non-comment character is found.
peekRelevant :: Parser (Maybe Char)
peekRelevant = do
    c <- peek
    case c of
        Nothing -> return Nothing
        Just c -> case c of
            '#' -> lineComment >> peekRelevant
            c | c `elem` " \n\t()" -> pop >> peekRelevant
            c -> return $ Just c
    where
        lineComment = do
            c <- peek
            case c of
                Nothing -> return ()
                Just '\n' -> return ()
                Just _ -> pop >> lineComment

-- Like `pop`, but keep popping until a non-whitespace non-comment character is found.
popRelevant = do
    a <- peekRelevant
    pop
    return a

-- Return all expected characters for an expression. Depends on the parse mode.
expressionChars :: Parser String
expressionChars = do
    state <- State.get
    let special = case getParseMode state of
            NormalMode -> ""
            DebugMode -> "?"
            InteractiveMode -> "?_"
    return $ special ++ "+-*=0123456789"

throwError :: String -> ErrParser a
throwError message = do
    state <- lift State.get
    Except.throwE (getLineNo state, getColNo state, message)

unexpectedError :: [Char] -> String -> ErrParser a
unexpectedError expected recieved =
    throwError $ concat ["Expected one of ", expected, ", got ", recieved]

-- Determine whether the parse mode is above a certain level.
levelAbove :: ParseMode -> Parser Bool
levelAbove mode = do
    state <- State.get
    return $ getParseMode state >= mode

parseExpression :: ErrParser Expression
parseExpression = do
    c <- lift popRelevant
    case c of
        Nothing -> expressionError "EOF"
        Just c -> case c of
            '+' -> Add <$> parseExpression <*> parseExpression
            '-' -> Negate <$> parseExpression
            '*' -> Deref <$> parseExpression
            '=' -> Normalize <$> parseExpression
            -- Numbers wouldn't contain a leading 0 anyways, so it is parsed separately.
            '0' -> return $ Literal 0
            c | c `elem` digit -> lift $ do
                rest <- parseAlphabet digit
                let digits = c:rest
                return . Literal $ read digits
            '?' -> do
                enabled <- lift $ levelAbove DebugMode
                if enabled then DebugWrite <$> parseExpression
                else expressionError "?"
            '_' -> do
                enabled <- lift $ levelAbove InteractiveMode
                if enabled then do
                    name <- lift $ parseAlphabet uppercase
                    return . Hole $ if null name then Nothing else Just name
                else expressionError "_"
            other -> expressionError [other]
    where
        expressionError :: String -> ErrParser a
        expressionError recieved = do
            expected <- lift expressionChars
            unexpectedError expected recieved

-- Return all expected characters for an expression.
instructionChars :: Parser String
instructionChars = do
    state <- State.get
    let special = case getParseMode state of
            NormalMode -> ""
            DebugMode -> "!"
            InteractiveMode -> "!|"
    alt <- expressionChars
    return $ special ++ lowercase ++ alt

parseInstruction :: ErrParser (Maybe Instruction)
parseInstruction = do
    c <- lift peekRelevant
    case c of
        Nothing -> return Nothing
        Just c -> Just <$> case c of
            c | c `elem` lowercase -> do
                name <- lift $ parseAlphabet lowercase
                test <- parseExpression
                return $ Label name test
            '!' -> do
                lift pop
                enabled <- lift $ levelAbove DebugMode
                if enabled then return DebugTape
                else instructionError "!"
            '|' -> do
                lift pop
                enabled <- lift $ levelAbove InteractiveMode
                if enabled then return Breakpoint
                else instructionError "|"
            c -> do
                dest <- Except.catchE parseExpression (\_ -> instructionError [c])
                nextChar <- lift peek
                case nextChar of
                    Nothing -> do
                        enabled <- lift $ levelAbove InteractiveMode
                        if enabled then return $ DebugPrintExpr dest
                        else instructionError "EOF"
                    Just _ -> do
                        src <- parseExpression
                        return $ Assign dest src
    where
        instructionError :: String -> ErrParser a
        instructionError recieved = do
            expected <- lift instructionChars
            unexpectedError expected recieved

makeErrorMsg :: Located ErrorMsg -> String
makeErrorMsg (line, col, msg) = concat [ "Syntax error at line "
                                       , show $ line + 1
                                       , ", column "
                                       , show $ col + 1
                                       , ": "
                                       , msg ]

parse :: ParseMode -> String -> Either String Program
parse mode code = case parsed of
        Left e -> Left $ makeErrorMsg e
        Right is -> Right $ Vector.fromList is
    where
        parseInstructions :: ErrParser [Instruction]
        parseInstructions = do
            instruction' <- parseInstruction
            case instruction' of
                Nothing -> return []
                Just i -> (i:) <$> parseInstructions
        parsed :: Either (Located ErrorMsg) [Instruction]
        parsed = State.evalState (Except.runExceptT parseInstructions) $ ParseState {
                getParseMode = mode,
                getLineNo = 0,
                getColNo = 0,
                getRest = code
            }