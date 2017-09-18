module SubMinimaPrettyPrinter where

import SubMinimaAST

prettyPrinter :: SubExpressionSemantics String ()
prettyPrinter = SubExpressionSemantics {
  foldStringLiteral = contextFree id,
  foldNumberLiteral = contextFree show
}

debuggingPrettyPrinter :: SubExpressionSemantics (IO String) ()
debuggingPrettyPrinter = SubExpressionSemantics {
  foldStringLiteral = contextFree (\text -> do { putStrLn ("Folding string " ++ text); return text }),
  foldNumberLiteral = contextFree (\num -> do { putStrLn ("Folding number " ++ show num); return (show num)})
}

-- note that we need to have a top-level IO something, so we need to extract that part of the tuple
debug :: SubExpression -> IO String
debug subexp = fst $ foldSubExpression debuggingPrettyPrinter () subexp
