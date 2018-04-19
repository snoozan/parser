module Main where

import           Control.Monad
import           Debug.Trace
import           Parser
import           System.Environment
import           System.IO

main :: IO ()
main = do
  args <- getArgs
  if null args
    then do
      line <- getLine
      if null line
        then return ()
        else do
          print (parseString line)
          main
    else do
      if "file" == (head args)
        then do
          ast <- parseFile (head (tail args))
          print (ast)
        else do
          return ()
