module Interpreter (runProgram, runConsole, interactiveConsole) where

-- State monads, which form the Momema core interpreter type.
import Control.Monad.Trans.State.Lazy (StateT (StateT))
import qualified Control.Monad.Trans.State.Lazy as State
-- Import `lift` for lifting IO to Momema.
import Control.Monad.Trans.Class (lift)

-- Represent the command list as a vector.
import qualified Data.Vector as Vector
import Data.Vector (Vector)

-- The tape, jump cache, and hole cache are all Maps.
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

-- All interpreter-generated output (debug prompts, etc.) is sent to STDERR.
import System.IO (stderr, hPutStr, hPutStrLn)

-- Various utility functions.
import Data.List (sortOn, intercalate, elemIndex, findIndices)
import Data.Maybe (maybeToList, fromJust)

-- Import the Instruction and Expression types from Base.
import Base (Instruction (..), Expression (..), Program)

import Parse (parse)
import qualified Parse

-- A jump label.
type Label = String
-- An index to a command.
type Position = Int
-- The cache for jump labels.
type JumpTable = Map Label [Position]
-- The tape of unbounded integers.
type Tape = Map Integer Integer

-- A hole label.
type HoleLabel = String
-- Known values of labeled holes.
type HoleCache = Map HoleLabel Integer

-- The state a Momema program holds.
data MomemaState = MomemaState {
    getTape :: Tape,
    getInstructions :: Program,
    getIp :: Position,
    getJumpTable :: JumpTable,
    getHoleCache :: HoleCache,
    didModifyTape :: Bool
}
type Momema = StateT MomemaState IO

startState :: Program -> MomemaState
startState prog = MomemaState {
    getTape = Map.empty,
    getInstructions = prog,
    getIp = 0,
    getJumpTable = Map.empty,
    getHoleCache = Map.empty,
    didModifyTape = False
}

-- Repeat a Momema action until it returns False.
doWhile :: Monad m => m Bool -> m ()
doWhile action = do
    continue <- action
    if continue then doWhile action
    else return ()

-- Try to read a string and return Nothing if it doesn't parse.
tryRead :: Read a => String -> Maybe a
tryRead string = case reads string of
    [(result, "")] -> Just result
    _ -> Nothing

-- A safe verstion of toEnum that returns a Maybe instead of raising an exception, courtesy of
-- https://stackoverflow.com/questions/2743858/safe-and-polymorphic-toenum
tryToEnum :: Enum a => Int -> Maybe a
tryToEnum i
    | a <= i, i <= z = Just $ toEnum i
    | otherwise = Nothing
    where
        a = fromEnum $ minBound `asTypeOf` i
        z = fromEnum $ maxBound `asTypeOf` i

-- Return the character code of a character read from STDIN.
getCharCode :: IO Integer
getCharCode = toInteger . fromEnum <$> getChar

-- Print a prompt to STDERR and return a line read from STDIN.
stderrPrompt :: String -> IO String
stderrPrompt prompt = hPutStr stderr prompt >> getLine

-- Keep running a `getInput` action until the validate function returns a Just.
readUntil :: (a -> Maybe b) -> IO a -> IO b
readUntil validate getInput = do
    input <- getInput
    case validate input of
        Just b -> return b
        Nothing -> readUntil validate getInput

-- Look through STDIN for a line containing a decimal integer.
getDecimalInteger :: IO Integer
getDecimalInteger = readUntil tryRead getLine

-- Retrieve the value of an integer on the tape.
deref :: Integer -> Momema Integer
deref (-9) = lift getCharCode
deref (-8) = lift getDecimalInteger
deref index = do
    state <- State.get
    case Map.lookup index $ getTape state of
        Just a -> return a
        Nothing -> return 0

-- Normalize an integer to 0 or 1.
normalize :: Integer -> Integer
normalize 0 = 0
normalize _ = 1

groupRuns :: [(Integer, Integer)] -> [(Integer, [Integer])]
groupRuns = foldr (\(k, v) l -> case l of
    (k', vs):xs | k' == k + 1 -> ((k, v:vs):xs)
    xs -> (k, [v]):xs) []

-- Generate a string representation of the tape.
-- Show the indices and values of every cell that has been assigned to before, in the format
-- `index:value`, by order of index. If the index would differ from the previous one by only 1,
-- represent it simply as `value`. If the index would differ by more than one, use a `..` instead
-- of a comma.
displayTape :: Tape -> String
displayTape tape = "[" ++ intercalate " .. " (map displayRun runs) ++ "]"
    where
        indices :: [(Integer, Integer)]
        indices = sortOn fst $ Map.toList tape
        
        runs :: [(Integer, [Integer])]
        runs = groupRuns indices
        
        displayRun :: (Integer, [Integer]) -> String
        displayRun (i, xs) = show i ++ ":" ++ intercalate "," (map show xs)

-- Write debug information to STDERR.
debug :: Momema ()
debug = do
    state <- State.get
    lift . hPutStrLn stderr $ "at position " ++ show (getIp state)
    lift . hPutStrLn stderr . displayTape $ getTape state

-- Write debug information and the argument to STDERR, then return it.
debugArg :: Integer -> Momema Integer
debugArg a = do
    debug
    lift . hPutStrLn stderr $ "argument: " ++ show a
    return a

getAnonymousHole :: IO Integer
getAnonymousHole = readUntil tryRead $ stderrPrompt "enter value for hole: "

getNamedHole :: String -> Momema Integer
getNamedHole name = do
    state <- State.get
    let holeCache = getHoleCache state
    case Map.lookup name holeCache of
        Just a -> return a
        Nothing -> do        
            result <- lift . readUntil tryRead . stderrPrompt $
                "enter value for hole " ++ show name ++ ": "
            State.put state { getHoleCache = Map.insert name result holeCache }
            return result

evalExpression :: Expression -> Momema Integer
evalExpression expr = case expr of
    Add a b -> (+) <$> evalExpression a <*> evalExpression b
    Negate a -> negate <$> evalExpression a
    Deref a -> evalExpression a >>= deref
    Normalize a -> normalize <$> evalExpression a
    Literal a -> return a
    DebugWrite a -> evalExpression a >>= debugArg
    Hole Nothing -> lift getAnonymousHole
    Hole (Just a) -> getNamedHole a

-- Index into a list cyclically (i.e. an index greater than the length of the list wraps around to
-- the beginning).
cyclicIndex :: [a] -> Integer -> a
cyclicIndex xs i = xs !! (fromInteger i `mod` length xs)

-- Get all the indices of jump instructions with a given label in the program. Read from the cache
-- if possible.
getLabelPositions :: Label -> Momema [Position]
getLabelPositions label = do
    state <- State.get
    let jumpTable = getJumpTable state
    case Map.lookup label jumpTable of
        Just a -> return a
        Nothing -> do
            let jumps = findIndices (\instruction -> case instruction of
                    Label label' _ | label' == label -> True
                    _ -> False) . foldr (:) [] $ getInstructions state
            State.put state { getJumpTable = Map.insert label jumps jumpTable }
            return jumps

jumpForward :: Label -> Integer -> Momema ()
jumpForward label amount = do
    positions <- getLabelPositions label
    state <- State.get
    let ip = getIp state
        ipIndex = toInteger . fromJust $ elemIndex ip positions
    State.put state { getIp = cyclicIndex positions $ amount + ipIndex }

-- Every line executed by the console is in an entirely separate Momema instance. This is so that
-- `didModifyTape` can work properly. The only state that carries over from the original Momema
-- program is the tape and hole cache, and the only state that carries back is the tape, hole
-- cache, and `didModifyTape`.
console :: Momema ()
console = do
        initState <- State.get
        let initTape = getTape initState
            initHoleCache = getHoleCache initState
        doWhile $ do
            line <- lift $ stderrPrompt "> "
            case line of
                ":quit" -> return False
                ":revert" -> do
                    State.put initState
                    return False
                ':':command -> do
                    lift . hPutStrLn stderr $ "command `:" ++ command ++ "` not recognized"
                    return True
                code -> do
                    case parse Parse.InteractiveMode code of
                        Left err -> lift $ hPutStrLn stderr err
                        Right program -> do
                            initState <- genInitState program
                            endState <- lift $ runMomema initState
                            State.put endState
                            if didModifyTape endState then
                                lift . hPutStrLn stderr . displayTape $ getTape endState
                            else return ()
                    return True
    where
        genInitState :: Program -> Momema MomemaState
        genInitState program = do
            state <- State.get
            return MomemaState {
                getTape = getTape state,
                getInstructions = program,
                getIp = 0,
                getJumpTable = Map.empty,
                getHoleCache = getHoleCache state,
                didModifyTape = False
            }

assign :: Integer -> Integer -> Momema ()
assign (-9) char = lift . putStr . maybeToList . tryToEnum $ fromInteger char
assign (-8) num = lift . putStr $ show num
assign dest src = do
    state <- State.get
    State.put state { getTape = Map.insert dest src $ getTape state, didModifyTape = True }

evalInstruction :: Instruction -> Momema ()
evalInstruction i = case i of
    Label label expr -> evalExpression expr >>= jumpForward label
    Assign a b -> do
        a' <- evalExpression a
        b' <- evalExpression b
        assign a' b'
    DebugTape -> debug
    Breakpoint -> do
        lift $ hPutStrLn stderr "breakpoint"
        console
    DebugPrintExpr a -> do
        a' <- evalExpression a
        lift . hPutStrLn stderr $ show a'

tick :: Momema Bool
tick = do
    state <- State.get
    let ip = getIp state
        instruction = getInstructions state Vector.!? ip
    case instruction of
        Nothing -> return False
        Just i -> do
            evalInstruction i
            state <- State.get
            State.put state { getIp = ip + 1 }
            return True

runMomema :: MomemaState -> IO MomemaState
runMomema = State.execStateT run
    where
        run :: Momema ()
        run = do
            continue <- tick
            if continue then run
            else return ()

-- Run a program.
runProgram :: Program -> IO ()
runProgram prog = do
    runMomema $ startState prog
    return ()

-- Run a program and then launch the interactive console.
runConsole :: Program -> IO ()
runConsole prog = do
    finalState <- runMomema $ startState prog
    State.execStateT console finalState
    return ()

-- The interactive console starts with a blank program.
interactiveConsole :: IO ()
interactiveConsole = runConsole Vector.empty