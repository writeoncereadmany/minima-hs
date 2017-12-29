module Minima where

import Data.Map (Map)
import qualified Data.Map as Map

import NanoParse
import MinimaAST
import MinimaParser
import MinimaTerpreter

initialContext :: Context
initialContext = (success, Map.fromList [("success", success), ("print", printFunction)], return ())

eval :: Context -> String -> Context
eval context code = foldl (foldExpression evaluator) context (parseProgram code)
