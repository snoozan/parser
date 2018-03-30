module Main where

import           Control.Monad
import           Debug.Trace
import           Parser
import           System.IO

main :: IO ()
main = do
  ast <- parseFile "test.txt"
  traceM ("ast: " ++ show ast)
