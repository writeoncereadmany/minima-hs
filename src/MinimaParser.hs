module MinimaParser where

import Data.Char
import Control.Applicative
import Control.Monad
import NanoParse
import MinimaAST

letter :: Parser Char
letter = satisfy isAlpha

identifier :: Parser String
identifier = do
  initial <- letter
  rest <- many $ satisfy isAlphaNum
  return $ initial:rest

variable :: Parser Expression
variable = Variable <$> token identifier

stringLiteral :: Parser Expression
stringLiteral = StringLiteral <$> token (enclosed (many $ noneOf "'") "'" "'")

intPart :: Parser String
intPart = do
  sign <- string "-" <|> return ""
  integralPart <- some $ satisfy isDigit
  return (sign ++ integralPart)

decimalPart :: Parser (String -> String)
decimalPart = do
  point <- string "."
  decimals <- some $ satisfy isDigit
  return ( ++ (point ++ decimals))

numberLiteral :: Parser Expression
numberLiteral = NumberLiteral <$> read <$> token (intPart `andMaybe` decimalPart)

keyValuePair :: Parser (String, Expression)
keyValuePair = do
  key <- token identifier
  reserved ":"
  value <- token expression
  return (key, value)

declaration :: Parser Expression
declaration = do
  name <- token identifier
  reserved "is"
  value <- expression
  return $ Declaration name value

object :: Parser Expression
object = Object <$> (enclosed (separated keyValuePair "," <|> return []) "{" "}")

group :: Parser Expression
group = Group <$> (enclosed (separated expression ",") "(" ")")

separated :: Parser a -> String -> Parser [a]
separated p sep = do { a <- p; rest [a] }
  where rest a = (do reserved ","
                     next <- p
                     rest (a ++ [next]))
                 <|> return a

call :: Parser (Expression -> Expression)
call = do
  arguments <- enclosed (separated expression "," <|> return []) "[" "]"
  return (flip Call arguments)

function :: Parser Expression
function = do
  arguments <- enclosed (separated identifier "," <|> return []) "[" "]"
  reserved "=>"
  body <- expression
  return (Function arguments body)

access :: Parser (Expression -> Expression)
access = do
  reserved ":"
  field <- identifier
  return (flip Access field)

expression :: Parser Expression
expression = (declaration <|> variable <|> stringLiteral <|> numberLiteral <|> function <|> object <|> group) `andMaybe` (access <|> call)

program :: Parser Program
program = separated expression ","

run :: String -> Program
run = runParser program
