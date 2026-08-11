module Parser (parseExpr) where

import SyntaxTree
import Text.Parsec
import Text.Parsec.String (Parser)

variable :: Parser Expr
variable = Var <$> many1 letter

lambda :: Parser Expr
lambda = do
  char '\\' <|> char 'λ'
  v <- many1 letter
  char '.'
  body <- expr
  return (Lam v body)

atom :: Parser Expr
atom = lambda
   <|> variable
   <|> between (char '(') (char ')') expr

application :: Parser Expr
application = do
  atoms <- many1 atom
  return (foldl1 App atoms)

expr :: Parser Expr
expr = application

parseExpr :: String -> Either ParseError Expr
parseExpr = parse expr ""