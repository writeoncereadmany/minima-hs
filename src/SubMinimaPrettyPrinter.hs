module SubMinimaPrettyPrinter where

import SubMinimaAST
import Data.List

testAST :: SubExpression
testAST = SubGroup [ SubStringLiteral "hello", SubNumberLiteral 42, SubStringLiteral "cheese" ]

-- prettyPrinter :: SubExpressionSemantics String ()
-- prettyPrinter = SubExpressionSemantics {
--   foldStringLiteral = contextFree id,
--   foldNumberLiteral = contextFree show,
--   foldGroup = contextFree (intercalate ", ")
-- }

debuggingPrettyPrinter :: SubExpressionSemantics String (IO ())
debuggingPrettyPrinter = SubExpressionSemantics {
  foldStringLiteral = \c -> \t -> (t, c >> putStrLn "folding string"),
  foldNumberLiteral = \c -> \n -> (show n, c >> putStrLn "folding number"),
  foldGroup = \c -> \(exps, newC) -> (last exps, newC)
}

-- note that we need to have a top-level IO something, so we need to extract that part of the tuple
debug :: SubExpression -> IO ()
debug subexp = snd $ foldSubExpression debuggingPrettyPrinter (return ()) subexp
