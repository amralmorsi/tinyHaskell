module Main (main, Stage, Error, printError, StageErrorOrOutput) where

import Lib
import TinyPrelude
import Control.Monad ((>=>))
import Data.Char (toLower)
import qualified Lexer
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Text.Pretty.Simple (pShow, pPrint, pPrintString)

{-
-- Key Function Signatures:
--  - pShow :: String -> Text
-}

{-
-- The compiler is divided into `Stage`s (Lexing, Parsing, Type-checking, etc), where each `Stage` either outputs a `String` or `Error`. 
-}


-- For compilation stage control and error handling --



-- Side Note: `Error` type could be implement as a classtype instead. 
-- Will revisit it to see which is better later.
{-
data ErrorMessage = String 
data Error = LexerError ErrorMessage | ParserError ErrorMessage | TypeError ErrorMessage deriving (Show, Eq)
-}
-- Making `errorMessage` of type `String` for now; could be changed later to parametatized type concstuctors 
-- for different types of error message
data Error a = LexerError {errorMessage :: a} 
           | ParserError {errorMessage :: a} 
           | TypeError {errorMessage :: a}
           | CCompilerError {errorMessage :: a}
           deriving (Eq)


instance Show Error where
    show (LexerError errorMessage) = "Lexer Error: \n" ++ (show.pShow) errorMessage ++ "\n"
    show (ParserError errorMessage) = "Parser Error: \n" ++ (show.pShow) errorMessage ++ "\n"
    show (TypeError errorMessage) = "Type Error: \n" ++ (show.pShow) errorMessage ++ "\n"
    show (CCompilerError errorMessage) = "GCC Error: \n" ++ (show.pShow) errorMessage ++ "\n"

printError :: Error -> IO ()
printError x = putStr $ show x

---------- # Stage Abstraction ----------
{-
-- Side Notes: 
--  1. `Stage` probably better be implemented as a typeclass, where `runStage` 
--      and other functions are part of its implementation.
-}

-- data Stage input output = Lexer {runLexer :: input -> StageErrorOrOutput output}
--                | Parser {runParser :: input -> StageErrorOrOutput output}
--                | TypeChecker {runTypeChecker :: input -> StageErrorOrOutput output}
--                | GCC {runGCC :: input -> StageErrorOrOutput output}
--                deriving (Show, Eq)


-- data StageType = Lexer
--                | Parser
--                | TypeChecker
--                | GCC
               
data Stage input output = Stage {
                                 stageName :: String,
                                 runStage :: input -> StageErrorOrOutput output
                                 } 
                          

data StageErrorOrOutput output = Right {stageOutput :: output }
                               | Left {stageError :: Error}


-- runStage :: Stage input output -> (input -> StageErrorOrOutput output)
-- runStage (Stage stageType stageRunner) = stageRunner

runAndPrintStage :: Show o => Stage i o -> i -> IO ()
runAndPrintStage (Stage name run) i = case run i of
                               Right o -> do 
                                          putStrLn name++": "
                                          pPrint o

                               Left error -> do
                                             printError error
                                             exitFailure

lexerStage :: Stage Sting (StageErrorOrOutput [Lexer.Token])
lexerStage = Stage "Lexer" Lexer.lexer

runToStage :: String -> Maybe (IO())
runToStage "lex" = lexerStage |> runAndPrintStage |> Just
runToStage _ = Nothing



-- A function that pipes/compose a `Stage` into another.
(>->) :: Stage i o -> Stage o o' -> Stage i o'
(>->) (Stage stage1 runStage1) (Stage stage2 runStage2) =  Stage (stage1 ++ " >-> " ++ stage2) (runStage1 >>> runStage2)

---------- # CLI Interface ----------
data Args = Args {filePath :: FilePath, 
                  toStage  :: String -> IO()}

-- Not sure how or why "Args File <$> runToStage (map toLower stageName)" output a `Maybe (Args String _IO()_)`
-- instead of `Maybe (Args String _(String -> IO ()_)) 
parseArgs :: [String] -> Maybe Args
parseArgs (stageName : file : _) = Args file <$> runToStage (map toLower stageName)
parseArgs _ = Nothing

run :: Args -> IO ()
run (Args filePath toStage) = do
  file <- readFile filePath
  toStage file

main :: IO ()
main = do
  stringArgs <- getArgs
  case parseArgs stringArgs of
    Nothing -> putStrLn "Invalid arguments"
    Just args -> run args
