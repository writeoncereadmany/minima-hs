module MinimaTerpreter where

import MinimaAST
import Data.Map (Map)
import qualified Data.Map as Map

data Type = TString | TNumber | TSuccess | TObject deriving (Show, Eq)

data Value
  = VObject Type String (Map String Value)
  | VBuiltinFunction String ([Value] -> Value)
  | VFunction [String] Expression

instance Show Value where
  show (VObject typ value methods) = show typ ++ ": " ++ value
  show (VBuiltinFunction name f) = "Function " ++ name
  show (VFunction params body) = "Custom function"

type Environment = Map String Value

vString :: String -> Value
vString text = VObject TString text Map.empty

vNumber :: Double -> Value
vNumber number = VObject TNumber (show number) Map.empty

vObject :: Map String Value -> Value
vObject fields = VObject TObject (show fields) fields

success :: Value
success = VObject TSuccess "success" Map.empty

callFunction :: Environment -> Value -> [Value] -> Value
callFunction context (VObject typ _ _) args = error "Cannot call an object"
callFunction context (VBuiltinFunction name f) args = f args
callFunction context (VFunction params body) args = let fVariables = Map.fromList $ zip params args
                                                        fContext = Map.union fVariables context
                                                     in fst $ foldExpression evaluator fContext body

access :: Value -> String -> Value
access (VObject _ _ fields) field = fields Map.! field
access _ _ = error "Cannot access fields of a function"

evaluator :: ExpressionSemantics Value Environment
evaluator = ExpressionSemantics {
  foldVariable = \context -> \name -> (context Map.! name, context),
  foldDeclaration = \context -> \name -> \value -> (success, Map.insert name value context),
  foldStringLiteral = contextFree vString,
  foldNumberLiteral = contextFree vNumber,
  foldCall = usingContext callFunction,
  foldFunction = contextFree2 VFunction,
  foldAccess = contextFree2 access,
  foldObject = contextFree $ vObject . Map.fromList,
  foldGroup = contextFree last
}

contextFree :: (a -> b) -> c -> a -> (b, c)
contextFree f = \context -> \initial -> (f initial, context)

contextFree2 :: (x -> y -> z) -> c -> x -> y -> (z, c)
contextFree2 f = \context -> \x -> \y -> (f x y, context)

usingContext :: (c -> x -> y -> z) -> c -> x -> y -> (z, c)
usingContext f = \context -> \x -> \y -> (f context x y, context)
