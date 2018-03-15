module Main where

import Parse (parse, unparse)
import Prim (primRules)
import Rewrite (evalProgram)

import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  mapM_ exec args

exec :: String -> IO ()
exec filename = do
  contents <- readFile filename
  case parse contents of
   Left err -> error (show err)
   Right program -> do
     let program' = evalProgram primRules program
     let text = unparse program'
     putStr text
  
