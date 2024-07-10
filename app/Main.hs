module Main where

import Befunge93 (interpret)
import System.Random (newStdGen)

main :: IO ()
main = do
  g <- newStdGen
  contents <- getContents
  putStrLn $ interpret g contents
