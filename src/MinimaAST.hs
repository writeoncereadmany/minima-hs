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

data ExpressionSemantics t = ExpressionSemantics {
  foldVariable :: String -> t,
  foldDeclaration :: String -> t -> t,
  foldStringLiteral :: String -> t,
  foldNumberLiteral :: Double -> t,
  foldCall :: t -> [t] -> t,
  foldFunction :: [String] -> Expression -> t,
  foldAccess :: t -> String -> t,
  foldObject :: [(String, t)] -> t,
  foldGroup :: [t] -> t
}

foldExpression :: ExpressionSemantics t -> Expression -> t
foldExpression with = foldOver where
  foldOver (Variable name) = foldVariable with name
  foldOver (Declaration name value) = foldDeclaration with name (foldOver value)
  foldOver (StringLiteral text) = foldStringLiteral with text
  foldOver (NumberLiteral number) = foldNumberLiteral with number
  foldOver (Call func args) = foldCall with (foldOver func) (foldOver <$> args)
  foldOver (Function params body) = foldFunction with params body
  foldOver (Access object field) = foldAccess with (foldOver object) field
  foldOver (Group expressions) = foldGroup with (foldOver <$> expressions)
