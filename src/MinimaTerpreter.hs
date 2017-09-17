module MinimaTerpreter where

import MinimaAST
import Data.Map (Map)
import qualified Data.Map as Map

data Value
  = VString String
  | VNumber Double
  | VObject (Map String Value)
  | VBuiltinFunction ([Value] -> Value)
  | VFunction [String] Expression

instance Show Value where
  show (VString s) = "String " ++ s
  show (VNumber n) = "Number " ++ show n
  show (VObject values) = "Object " ++ show values
  show (VBuiltinFunction f) = "Builtin function"
  show (VFunction params body) = "Custom function"

type Environment = Map String Value

evaluator :: ExpressionSemantics Value Environment
evaluator = ExpressionSemantics {
  foldVariable = \context -> \name -> error "variables not yet implemented",
  foldDeclaration = \context -> \name -> \value -> error "declarations not yet implemented",
  foldStringLiteral = contextFree VString,
  foldNumberLiteral = contextFree VNumber,
  foldCall = \context -> \fun -> \args -> error "calls not yet implemented",
  foldFunction = \context -> \params -> \body -> error "functions not yet implemented",
  foldAccess = \context -> \object -> \field -> error "access not yet implemented",
  foldObject = contextFree $ VObject . Map.fromList,
  foldGroup = \context -> \expressions -> error "groups not yet implemented"
}

contextFree :: (a -> b) -> c -> a -> (b, c)
contextFree f = \context -> \initial -> (f initial, context)
