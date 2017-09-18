module Main where

import Minima

main :: IO ()
main = do
  line <- getLine
  putStrLn $ show (eval line)
  main
