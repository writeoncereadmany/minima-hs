module Main where

import Minima

main :: IO ()
main = do
  line <- getLine
  eval line
  main
