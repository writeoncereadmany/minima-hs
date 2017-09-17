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
