module Main where

import Minima
import System.IO
import Data.Map (Map)
import qualified Data.Map as Map

main :: IO ()
main = do
  putStrLn "Welcome to Minima, version 0.1.0"
  withContext initialContext

withContext context = do
  putStr "> "
  hFlush stdout
  line <- getLine
  let (val, env, io) = eval context line
  io
  putStrLn (show val)
  withContext (val, Map.insert "it" val env, return ())
