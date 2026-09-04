module Parser (parseExpr) where

import SyntaxTree
import Text.Parsec
import Text.Parsec.String (Parser)

-- consume trailing whitespace after a token
lexeme :: Parser a -> Parser a
lexeme p = p <* spaces

symbol :: Char -> Parser Char
symbol c = lexeme (char c)

identChar :: Parser Char
identChar = alphaNum <|> char '_' <|> char '\''

variable :: Parser Expr
variable = lexeme (Var <$> many1 identChar)

lambda :: Parser Expr
lambda = do
  _ <- lexeme (char '\\' <|> char 'λ')
  vs <- many1 (lexeme (many1 identChar))
  _ <- symbol '.'
  body <- expr
  return (foldr Lam body vs)

atom :: Parser Expr
atom = lambda
   <|> variable
   <|> between (symbol '(') (symbol ')') expr

application :: Parser Expr
application = do
  atoms <- many1 atom
  return (foldl1 App atoms)

expr :: Parser Expr
expr = application

parseExpr :: String -> Either ParseError Expr
parseExpr = parse (spaces *> expr <* eof) ""