module MinimaTerpreter where

import MinimaAST
import Data.Map (Map)
import qualified Data.Map as Map

-- this kinda sucks - it's how we handle built-in types, but it's not exactly extensible
data State = Num Double | Str String | Nowt

data Value
  = VObject State String (Map String Value)
  | VBuiltinFunction String ([Value] -> Value)
  | VFunction [String] Expression

instance Show Value where
  show (VObject state display methods) = display
  show (VBuiltinFunction name f) = "Function " ++ name
  show (VFunction params body) = "Custom function"

type Environment = Map String Value

invalidArgs :: String -> String -> [Value] -> a
invalidArgs name expected actual = error (name ++ " takes " ++ expected ++ ": given " ++ show actual)

showAs :: String -> Value
showAs text = VBuiltinFunction "show" shower'
  where shower' [] = vString text
        shower' args = invalidArgs "show" "no arguments" args

vString :: String -> Value
vString text = VObject (Str text) text  (Map.fromList
    [ ("concat", VBuiltinFunction "concat" concatStrings)
    , ("show", showAs text)
    ])
    where concatStrings [(VObject (Str t) _ _)] = vString $ text ++ t
          concatStrings args = invalidArgs "Concat" "a single string" args

vNumber :: Double -> Value
vNumber number = VObject (Num number) (show number) (Map.fromList
    [ binaryOp "plus" (+)
    , binaryOp "minus" (-)
    , binaryOp "multiplyBy" (*)
    , binaryOp "divideBy" (/)
    , ("show", showAs $ show number)
    ])
    where apply _ op [(VObject (Num n) _ _)] = vNumber $ number `op` n
          apply name op args = invalidArgs name "a single number" args
          binaryOp name op = (name, VBuiltinFunction name (apply name op))

vObject :: Map String Value -> Value
vObject fields = VObject Nowt (show fields) fields

success :: Value
success = VObject Nowt "Success" ( Map.fromList [("show", showAs "Success")] )

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
  foldVariable = usingContext (Map.!),
  foldDeclaration = \context -> \name -> \value -> (success, Map.insert name value context),
  foldStringLiteral = contextFree vString,
  foldNumberLiteral = contextFree vNumber,
  foldCall = usingContext2 callFunction,
  foldFunction = contextFree2 VFunction,
  foldAccess = contextFree2 access,
  foldObject = contextFree $ vObject . Map.fromList,
  foldGroup = contextFree last
}
