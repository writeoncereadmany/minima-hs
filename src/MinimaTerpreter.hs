module MinimaTerpreter where

import MinimaAST
import Data.Map (Map)
import qualified Data.Map as Map

-- this kinda sucks - it's how we handle built-in types, but it's not exactly extensible
data State = Num Double | Str String | Nowt

data Value
  = VObject State String (Map String Value)
  | VBuiltinFunction String (IO () -> [Value] -> (Value, IO ()))
  | VFunction Environment [String] Expression

instance Show Value where
  show (VObject state display methods) = display
  show (VBuiltinFunction name f) = "Function " ++ name
  show (VFunction env params body) = "Custom function"

type Environment = Map String Value

type Context = (Value, Environment, IO ())

vString :: String -> Value
vString text = VObject (Str text) text Map.empty

vNumber :: Double -> Value
vNumber num = VObject (Num num) (show num) (Map.fromList
  [ binaryOperator "plus" (+)
  , binaryOperator "minus" (-)
  , binaryOperator "multiplyBy" (*)
  , binaryOperator "divideBy" (/)
  , ("show", VBuiltinFunction "show" (purely doShow))
  ]) where
    impl :: (Double -> Double -> Double) -> [Value] -> Value
    impl op [(VObject (Num other) _ _)] = vNumber $ num `op` other
    impl op _ = error "wrong args"
    doShow :: [Value] -> Value
    doShow [] = vString $ show num
    doShow _ = error "wrong args"
    binaryOperator :: String -> (Double -> Double -> Double) -> (String, Value)
    binaryOperator name op = (name, VBuiltinFunction name (purely $ impl op))

purely :: ([Value] -> Value) -> (IO () -> [Value] -> (Value, IO ()))
purely f = \io -> \args -> (f args, io)

printFunction :: Value
printFunction = VBuiltinFunction "print" pf where
  pf io [(VObject (Str str) _ _)] = (success, io >> putStrLn str)
  pf _ _ = error "wrong args"

vObject :: [(String, Context)] -> Value
vObject contexts = VObject Nowt (show fields) fields where
        extractValues (s, (v, c, io)) = (s, v)
        fields = Map.fromList $ extractValues <$> contexts

success :: Value
success = VObject Nowt "success" Map.empty

access :: Context -> Context -> String -> Context
access (_, env, _) (obj, _, io) field = (getField obj field, env, io) where
  getField (VObject _ _ fields) field = fields Map.! field
  getField func _ = error ("Cannot get field from " ++ (show func))

ioFromList :: IO () -> [Context] -> IO ()
ioFromList io [] = io
ioFromList io cs = ioFrom $ last cs

call :: Context -> Context -> [Context] -> Context
call (_, env, _) (fun, _, fio) args = let (result, io) = doCall fun args in (result, env, io) where
  doCall (VBuiltinFunction _ f) args = f (ioFromList fio args) (valueFrom <$> args)
  doCall (VFunction fenv params body) args = let variables = Map.fromList $ zip params (valueFrom <$> args)
                                                 newEnv = Map.union variables fenv
                                                 ioIn = ioFromList fio args
                                                 (result, _, ioOut) = foldExpression evaluator (success, newEnv, ioIn) body
                                              in (result, ioOut)
  doCall (VObject _ _ _) _ = error "Cannot call an object"

evaluator :: ExpressionSemantics Context
evaluator = ExpressionSemantics {
  foldVariable = usingEnv (Map.!),
  foldDeclaration = \(_, env, _) -> \name -> \(value, _, io) -> (success, Map.insert name value env, io),
  foldStringLiteral = contextFree vString,
  foldNumberLiteral = contextFree vNumber,
  foldCall = call,
  foldFunction = \(_, env, io) -> \params -> \body -> (VFunction env params body, env, io),
  foldAccess = access,
  foldObject = \(_, env, io) -> \fields -> (vObject fields, env, ioFromList io (snd <$> fields)),
  -- a group cannot be empty, according to the grammar
  foldGroup = \(_, env, io) -> \groupedElements -> (valueFrom $ last groupedElements, env, ioFrom $ last groupedElements)
}

valueFrom :: (Value, Environment, IO ()) -> Value
valueFrom (v, _, _) = v

envFrom :: (Value, Environment, IO ()) -> Environment
envFrom (_, e, _) = e

ioFrom :: (Value, Environment, IO ()) -> IO ()
ioFrom (_, _, io) = io

contextFree :: (a -> Value) -> Context -> a -> Context
contextFree f (v, c, io) a = (f a, c, io)

contextFree2 :: (a -> b -> Value) -> Context -> a -> b -> Context
contextFree2 f (v, c, io) a b = (f a b, c, io)

usingEnv :: (Environment -> a -> Value) -> Context -> a -> Context
usingEnv f (v, c, io) a = (f c a, c, io)
