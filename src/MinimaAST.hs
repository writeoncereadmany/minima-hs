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

data ExpressionSemantics t c = ExpressionSemantics {
  foldVariable :: c -> String -> (t, c),
  foldDeclaration :: c -> String -> t -> (t, c),
  foldStringLiteral :: c -> String -> (t, c),
  foldNumberLiteral :: c -> Double -> (t, c),
  foldCall :: c -> t -> [t] -> (t, c),
  foldFunction :: c -> [String] -> Expression -> (t, c),
  foldAccess :: c -> t -> String -> (t, c),
  foldObject :: c -> [(String, t)] -> (t, c),
  foldGroup :: c -> [t] -> (t, c)
}

foldExpression :: ExpressionSemantics t c -> c -> Expression -> (t, c)
foldExpression with = foldOver where
  foldOver context (Variable name) = foldVariable with context name
  foldOver context (Declaration name value) = foldDeclaration with context name (fst $ foldOver context value)
  foldOver context (StringLiteral text) = foldStringLiteral with context text
  foldOver context (NumberLiteral number) = foldNumberLiteral with context number
  foldOver context (Call func args) = foldCall with context (fst $ foldOver context func) (fst <$> ((foldOver context) <$> args))
  foldOver context (Function params body) = foldFunction with context params body
  foldOver context (Access object field) = foldAccess with context (fst $ foldOver context object) field
  foldOver context (Object fields) = foldObject with context (map (\(n, x) -> (n, fst $ foldOver context x)) fields)
  foldOver context (Group expressions) = foldGroup with context (case expressions of
      (x:xs) -> fst $ foldGroup' (foldOver context x) xs foldOver where
        foldGroup' (initial, context) = foldGroup'' ([initial], context) where
          foldGroup'' (acc, context) (x:xs) folder = let (next, c) = folder context x
                                                      in foldGroup'' (acc ++ [next], c) xs folder
      [] -> error "Cannot have an empty group"
    )
