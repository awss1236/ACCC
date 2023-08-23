module Main where
import Control.Monad
import Data.Maybe
import qualified Data.Text as Txt
import System.Environment
import System.Process
import System.FilePath
import System.Exit

import CLexer
import CParser
import CAssembler
import PPrint

main :: IO()
main = do
  args <- getArgs
  if null args then do
    putStrLn "Expected source file. Instead got nothing."
    exitWith (ExitFailure 2)
  else do
    let sf = head args
    s <- readFile sf
    let mts = lexC s
    if isNothing mts then do
      putStrLn "Bruh yo code don't even lex right."
      exitWith (ExitFailure 2)
    else do
      let ts  = fromJust mts
      putStrLn "tokens:"
      print ts
      let mpts= parse ts
      if isNothing mpts then do
        putStrLn "Na uh not parsin."
        exitWith (ExitFailure 2)
      else do
        let pts = fromJust mpts
        putStrLn "\nAST:"
        putStrLn $ pPrintProg pts
        putStrLn "\nASM:"
        putStrLn $ genProgAsm pts

        let asf = sf -<.> "s"
        writeFile asf (genProgAsm pts)
        runCommand $ "gcc -m32 " ++ asf ++ " -o " ++ (takeDirectory sf </> "a.out")
        exitSuccess
