module Minima where

import Data.Map (Map)
import qualified Data.Map as Map

import NanoParse
import MinimaAST
import MinimaParser
import MinimaTerpreter

eval :: String -> IO ()
eval code = ioFrom $ foldl (foldExpression evaluator) (success, Map.fromList [("success", success), ("print", printFunction)], return ()) (parseProgram code)
