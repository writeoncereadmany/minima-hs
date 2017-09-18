module MinimaAST where

type Program = [Expression]

data Expression
  = Variable String
  | Declaration String Expression
  | StringLiteral String
  | NumberLiteral Double
  | Call Expression [Expression]
  | Function [String] Expression
  | Access Expression String
  | Object [(String, Expression)]
  | Group [Expression]
  deriving (Show)

data ExpressionSemantics c = ExpressionSemantics {
  foldVariable :: c -> String -> c,
  foldDeclaration :: c -> String -> c -> c,
  foldStringLiteral :: c -> String -> c,
  foldNumberLiteral :: c -> Double -> c,
  foldCall :: c -> c -> [c] -> c,
  foldFunction :: c -> [String] -> Expression -> c,
  foldAccess :: c -> c -> String -> c,
  foldObject :: c -> [(String, c)] -> c,
  foldGroup :: c -> [c] -> c
}

foldExpression :: ExpressionSemantics c -> c -> Expression -> c
foldExpression with = foldOver where
  foldOver context (Variable name) = foldVariable with context name
  foldOver context (Declaration name value) = foldDeclaration with context name (foldOver context value)
  foldOver context (StringLiteral text) = foldStringLiteral with context text
  foldOver context (NumberLiteral number) = foldNumberLiteral with context number
  foldOver context (Call func args) = foldCall with context (foldOver context func) ((foldOver context) <$> args)
  foldOver context (Function params body) = foldFunction with context params body
  foldOver context (Access object field) = foldAccess with context (foldOver context object) field
  foldOver context (Object fields) = foldObject with context (map (\(n, x) -> (n, foldOver context x)) fields)
  foldOver context (Group expressions) = foldGroup with context (build context expressions []) where
    build context [] acc = acc
    build context (x:xs) acc = let newContext = foldOver context x in build newContext xs (newContext : acc)
