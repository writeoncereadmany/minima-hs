module Minima where

import Data.Map (Map)
import qualified Data.Map as Map

import NanoParse
import MinimaAST
import MinimaParser
import MinimaTerpreter

eval :: String -> (Value, Environment)
eval code = foldl (foldExpression evaluator) (success, Map.fromList [("success", success)]) (parseProgram code) 
