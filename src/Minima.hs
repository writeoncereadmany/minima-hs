import Data.Map (Map)
import qualified Data.Map as Map

import NanoParse
import MinimaAST
import MinimaParser
import MinimaTerpreter

eval :: String -> (Value, Environment)
eval code = foldl execute (success, Map.fromList [("success", success)]) (parseProgram code) where
  execute (value, context) expr = foldExpression evaluator context expr
