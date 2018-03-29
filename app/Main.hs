module Main where 

import Parser
import System.IO
import Control.Monad

main :: IO ()
main = do
        handle <- openFile "test.txt" ReadMode
        contents <- hGetContents handle
        print (parseString contents)
        hClose handle

