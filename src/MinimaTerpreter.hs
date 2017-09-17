import MinimaAST
import Data.Map (Map)
import qualified Data.Map as Map

data Value
  = VString String
  | VNumber Double
  | VObject (Map String Value)
  | VBuiltinFunction ([Value] -> Value)
  | VFunction [String] Expression

type Environment = Map String Value

evaluator :: ExpressionSemantics Value Environment
evaluator = ExpressionSemantics {
  foldVariable = \context -> \name -> error "variable",
  foldDeclaration = \context -> \name -> \value -> error "declaration",
  foldStringLiteral = \context -> \text -> error "stringLiteral",
  foldNumberLiteral = \context -> \number -> error "numberLiteral",
  foldCall = \context -> \fun -> \args -> error "call",
  foldFunction = \context -> \params -> \body -> error "function",
  foldAccess = \context -> \object -> \field -> error "access",
  foldObject = \context -> \fields -> error "object",
  foldGroup = \context -> \expressions -> error "group"
}
