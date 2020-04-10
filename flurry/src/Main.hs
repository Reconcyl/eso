{-# LANGUAGE NamedFieldPuns #-}

import Data.Char (isDigit, chr, ord)

import System.Environment (getArgs, getProgName)
import System.IO (hPutStrLn, stderr)

import Text.Read (readMaybe)

import AST (parse)
import Interpreter (run)

getInputs :: String -> [Int]
getInputs s = read <$> getInputDigitStrings s
    where
        mapHead :: (a -> a) -> [a] -> [a]
        mapHead f (a:as) = f a : as
        mapHead _ []     = []

        getInputDigitStrings :: String -> [String]
        getInputDigitStrings (d1:d2:rest)
            | isDigit d1 || d1 == '-', isDigit d2
                = mapHead (d1:) $ getInputDigitStrings (d2:rest)
        getInputDigitStrings (d:rest)
            | isDigit d = [d] : getInputDigitStrings rest
            | otherwise = getInputDigitStrings rest
        getInputDigitStrings [] = []

data OutCfg = OutInt | OutByte | OutDebug | OutNone
data RetCfg = RetInt           | RetDebug | RetNone
data  InCfg =  InInt |  InByte |             InNone

data IOCfg = IOCfg { outCfg :: OutCfg, retCfg :: RetCfg, inCfg :: InCfg }

data Cfg = Cfg { io :: IOCfg, isLiteral :: Bool, codeArg :: String, inputArgs :: [Int] }

parseCfg :: [String] -> Maybe Cfg
parseCfg args = do
    (ioCfg, isLiteral, codeArg, inputArgs) <- case args of
        (('-' : ioCfgStr) : "-c" : code : inputs) -> Just (ioCfgStr, True,  code, inputs)
        (                   "-c" : code : inputs) -> Just ("ddn",    True,  code, inputs)
        (('-' : ioCfgStr) :        file : inputs) -> Just (ioCfgStr, False, file, inputs)
        (                          file : inputs) -> Just ("bnb",    False, file, inputs)
        _                                         -> Nothing
    io <- parseIoCfg ioCfg
    parsedArgs <- mapM readMaybe inputArgs
    return Cfg { io, isLiteral, codeArg, inputArgs = parsedArgs }
    where
        parseIoCfg :: String -> Maybe IOCfg
        parseIoCfg [o, r, i] = do
            outCfg <- case o of
                'i' -> Just OutInt
                'b' -> Just OutByte
                'd' -> Just OutDebug
                'n' -> Just OutNone
                _   -> Nothing
            retCfg <- case r of
                'i' -> Just RetInt
                'd' -> Just RetDebug
                'n' -> Just RetNone
                _   -> Nothing
            inCfg <- case i of
                'i' -> Just InInt
                'b' -> Just InByte
                'n' -> Just InNone
                _   -> Nothing
            return IOCfg { outCfg, retCfg, inCfg }
        parseIoCfg _ = Nothing

errLn :: String -> IO ()
errLn = hPutStrLn stderr

printHelp :: String -> IO ()
printHelp progName = do
    errLn "Usage:"
    errLn $ progName ++ " [io config] [filename] [inputs...]   " ++
               "run code stored at [filename] with [inputs...] on top of the stack"
    errLn $ progName ++ " [io config] -c [code]  [inputs...]   " ++
               "run [code] with [inputs...] on top of the stack"

main :: IO ()
main = do
    args <- getArgs
    case parseCfg args of
        Nothing -> getProgName >>= printHelp
        Just Cfg { io, isLiteral, codeArg, inputArgs } -> do
            code <- if isLiteral
                        then return codeArg
                        else readFile codeArg
            case parse code of
                Nothing -> errLn "Code could not be parsed."
                Just ast -> do
                    stdInputs <- case inCfg io of
                        InInt  -> getInputs <$> getContents
                        InByte -> map ord <$> getContents
                        InNone -> return []
                    let stack = stdInputs ++ inputArgs
                    let (returnVal, outputs) = run ast stack
                    case outCfg io of
                        OutInt   -> putStrLn $ unwords $ map show outputs
                        OutByte  -> putStrLn $ map chr outputs
                        OutDebug -> mapM_ (errLn . ("Output: " ++) . show) outputs
                        OutNone  -> return ()
                    case returnVal of
                        Nothing -> return ()
                        Just n  -> case retCfg io of
                            RetInt   -> print n
                            RetDebug -> errLn $ "Return: " ++ show n
                            RetNone  -> return ()
