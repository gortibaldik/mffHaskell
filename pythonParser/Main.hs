module Main where
import TokenParser
import SyntacticAnalysis
import SemanticAnalysis
import PrettyPrinter
import System.Environment
import System.Exit
import System.IO.Error
import Control.Exception
import Control.Monad
import Text.Megaparsec
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer
import Data.Functor.Identity
import Data.Void (Void)

checkArgs :: [String] -> IO ()
checkArgs arr = when (length arr /= 1) $ do
    putStrLn "Invalid number of arguments, expected 1 argument!"
    exitWith $ ExitFailure 1

fileExceptionHandler :: IOError -> IO ()
fileExceptionHandler e
    | isDoesNotExistError e = putStrLn "File does not exist!"
    | isPermissionError e = putStrLn "Doesn't have permissions to open the file"
    | isIllegalOperation e = putStrLn "Illegal operation with file"

processFile :: String -> IO ()
processFile file = do
    contents <- readFile file
    tokens <- eitherWrapper contents (tokenize "")
    ast <- eitherWrapper tokens analyzeSyntax
    semanticAnalysisWrapper ast
    putStrLn $ pplistshow ast
    return ()

eitherWrapper :: (VisualStream a, TraversableStream a) => a -> ( a -> Either (ParseErrorBundle a Void) b) -> IO b
eitherWrapper a f = do
    let either = f a
    case either of
        (Left errMsg) -> do
            putStrLn $ errorBundlePretty errMsg
            exitWith $ ExitFailure 2
        (Right output) -> return output

semanticAnalysisWrapper :: [Statement] -> IO ()
semanticAnalysisWrapper statements = do
    let errMsg = runIdentity $ execWriterT (runReaderT (readStatements statements) ["print", "read", "pass" ])
    if null errMsg then 
        return ()
    else do
        mapM_ putStrLn errMsg
        exitWith $ ExitFailure 2

main :: IO ()
main = do
    args <- getArgs
    checkArgs args
    processFile (head args) `catch` fileExceptionHandler
