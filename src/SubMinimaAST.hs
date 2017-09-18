module SubMinimaAST where

type SubProgram = [SubExpression]

data SubExpression
  = SubStringLiteral String
  | SubNumberLiteral Double
  -- | SubVariable String
  -- | SubDeclaration String SubExpression
  -- | SubCall SubExpression [SubExpression]
  -- | SubFunction [String] SubExpression
  -- | SubAccess SubExpression String
  -- | SubObject [(String, SubExpression)]
  | SubGroup [SubExpression]
  deriving (Show)

data SubExpressionSemantics t c = SubExpressionSemantics {
  foldStringLiteral :: c -> String -> (t, c),
  foldNumberLiteral :: c -> Double -> (t, c),
  -- foldVariable :: c -> String -> (t, c),
  -- foldDeclaration :: c -> String -> t -> (t, c),
  -- foldCall :: c -> t -> [t] -> (t, c),
  -- foldFunction :: c -> [String] -> SubExpression -> (t, c),
  -- foldAccess :: c -> t -> String -> (t, c),
  -- foldObject :: c -> [(String, t)] -> (t, c),
  foldGroup :: c -> ([t], c) -> (t, c)
}

foldSubExpression :: SubExpressionSemantics t c -> c -> SubExpression -> (t, c)
foldSubExpression with = foldOver where
  foldOver context (SubStringLiteral text) = foldStringLiteral with context text
  foldOver context (SubNumberLiteral number) = foldNumberLiteral with context number
  -- foldOver context (SubVariable name) = foldVariable with context name
  -- foldOver context (SubDeclaration name value) = foldDeclaration with context name (fst $ foldOver context value)
  -- foldOver context (SubCall func args) = foldCall with context (fst $ foldOver context func) (fst <$> ((foldOver context) <$> args))
  -- foldOver context (SubFunction params body) = foldFunction with context params body
  -- foldOver context (SubAccess object field) = foldAccess with context (fst $ foldOver context object) field
  -- foldOver context (SubObject fields) = foldObject with context (map (\(n, x) -> (n, fst $ foldOver context x)) fields)
  foldOver context (SubGroup expressions) = foldGroup with context (case expressions of
      (x:xs) -> foldGroup' (foldOver context x) xs foldOver where
        foldGroup' (initial, context) = foldGroup'' ([initial], context) where
          foldGroup'' (acc, context) (x:xs) folder = let (next, c) = folder context x
                                                      in foldGroup'' (acc ++ [next], c) xs folder
          foldGroup'' (acc, context) [] folder = (acc, context)
      [] -> error "Cannot have an empty group"
    )

contextFree :: (a -> b) -> c -> a -> (b, c)
contextFree f = \context -> \initial -> (f initial, context)

contextFree2 :: (x -> y -> z) -> c -> x -> y -> (z, c)
contextFree2 f = \context -> \x -> \y -> (f x y, context)

usingContext :: (c -> a -> b) -> c -> a -> (b, c)
usingContext f = \context -> \initial -> (f context initial, context)

usingContext2 :: (c -> x -> y -> z) -> c -> x -> y -> (z, c)
usingContext2 f = \context -> \x -> \y -> (f context x y, context)
