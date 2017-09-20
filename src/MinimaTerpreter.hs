module MinimaTerpreter where

import MinimaAST
import Data.Map (Map)
import qualified Data.Map as Map

-- this kinda sucks - it's how we handle built-in types, but it's not exactly extensible
data State = Num Double | Str String | Nowt

data Value
  = VObject State String (Map String Value)
  -- builtin functions have to be able to do IO things, and also return a value
  | VBuiltinFunction String ([Value] -> Value)
  | VFunction Environment [String] Expression

instance Show Value where
  show (VObject state display methods) = display
  show (VBuiltinFunction name f) = "Function " ++ name
  show (VFunction env params body) = "Custom function"

type Environment = Map String Value

-- for now, just value/environment, but this will also contain IO() in a bit
type Context = (Value, Environment)

vString :: String -> Value
vString text = VObject (Str text) text Map.empty

vNumber :: Double -> Value
vNumber num = VObject (Num num) (show num) (Map.fromList
  [ binaryOperator "plus" (+)
  , binaryOperator "minus" (-)
  , binaryOperator "multiplyBy" (*)
  , binaryOperator "divideBy" (/)
  ]) where
    impl :: (Double -> Double -> Double) -> [Value] -> Value
    impl op [(VObject (Num other) _ _)] = vNumber $ num `op` other
    impl op _ = error "wrong args"
    binaryOperator :: String -> (Double -> Double -> Double) -> (String, Value)
    binaryOperator name op = (name, VBuiltinFunction name (impl op))


vObject :: [(String, Context)] -> Value
vObject contexts = VObject Nowt (show fields) fields where
        extractValues (s, (v, c)) = (s, v)
        fields = Map.fromList $ extractValues <$> contexts

success :: Value
success = VObject Nowt "success" Map.empty

access :: Context -> Context -> String -> Context
access (_, env) (obj, _) field = (getField obj field, env) where
  getField (VObject _ _ fields) field = fields Map.! field
  getField func _ = error ("Cannot get field from " ++ (show func))

call :: Context -> Context -> [Context] -> Context
call (_, env) (fun, _) args = (doCall fun args, env) where
  doCall (VBuiltinFunction _ f) args = f (fst <$> args)
  doCall (VFunction fenv params body) args = let variables = Map.fromList $ zip params (fst <$> args)
                                                 newEnv = Map.union variables fenv
                                              in fst $ foldExpression evaluator (success, newEnv) body
  doCall (VObject _ _ _) _ = error "Cannot call an object"

evaluator :: ExpressionSemantics Context
evaluator = ExpressionSemantics {
  foldVariable = usingEnv (Map.!),
  foldDeclaration = \(_, env) -> \name -> \(value, _) -> (success, Map.insert name value env),
  foldStringLiteral = contextFree vString,
  foldNumberLiteral = contextFree vNumber,
  foldCall = call,
  foldFunction = \(_, env) -> \params -> \body -> (VFunction env params body, env),
  foldAccess = access,
  foldObject = contextFree vObject,
  foldGroup = \(_, env) -> \groupedElements -> (fst $ last groupedElements, env)
}

contextFree :: (a -> Value) -> Context -> a -> Context
contextFree f (v, c) a = (f a, c)

contextFree2 :: (a -> b -> Value) -> Context -> a -> b -> Context
contextFree2 f (v, c) a b = (f a b, c)

usingEnv :: (Environment -> a -> Value) -> Context -> a -> Context
usingEnv f (v, c) a = (f c a, c)
