import System.Environment (getArgs, getProgName)
import System.IO (stderr, hPutStr, hPutStrLn)

import Base (Program)
import qualified Interpreter
import qualified Parse

errLn :: String -> IO ()
errLn = hPutStrLn stderr

main = do
    args <- getArgs
    case args of
        ["--help"] -> helpMessage
        [fileName]       -> runFile Interpreter.runProgram Parse.NormalMode      fileName
        ["-d", fileName] -> runFile Interpreter.runProgram Parse.DebugMode       fileName
        ["-i", fileName] -> runFile Interpreter.runConsole Parse.InteractiveMode fileName
        [] -> do
            errLn "Momema: console"
            errLn "use `--help` for help."
            Interpreter.interactiveConsole
        args -> do
            errLn $ "Momema: arguments " ++ unwords args ++ " not recognized"
            errLn $ "use `--help` for more information."
    where
        runFile :: (Program -> IO ()) -> Parse.ParseMode -> String -> IO ()
        runFile runFunction mode fileName = do
            program <- readFile fileName
            case Parse.parse mode program of
                Left err -> hPutStrLn stderr err
                Right parsed -> runFunction parsed
        helpMessage :: IO ()
        helpMessage = do
            progName <- getProgName
            errLn "Momema: help"
            errLn $ "  " ++ progName ++ " --help        display this help message"
            errLn $ "  " ++ progName ++ " code.mma      execute file"
            errLn $ "  " ++ progName ++ " -d code.mma   execute file in debug mode"
            errLn $ "  " ++ progName ++ " -i code.mma   execute file in interactive mode"
            errLn $ "  " ++ progName ++ "               launch interactive console"