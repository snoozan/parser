module Main where

import           Control.Monad
import           Parser
import           System.IO
import           Debug.Trace

main :: IO ()
main = do
  ast <- parseFile "test.txt" 
  traceM("ast: " ++ show ast)
