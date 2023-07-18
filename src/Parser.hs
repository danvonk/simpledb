module Parser (program) where

import Sql
import Text.Parsec hiding (spaces)
import Text.ParserCombinators.Parsec.Prim hiding (try)

spaces :: Parser String
spaces = many space

spaces' :: Parser String
spaces' = many1 space

keyword :: String -> Parser String
keyword = string

parens :: Parser a -> Parser a
parens = between (char '(' <* spaces) (spaces *> char ')')

commas :: Parser a -> Parser [a]
commas p = sepBy1 p (spaces *> char ',' <* spaces)

name :: Parser String
name = (:) <$> letter <*> many alphaNum

sqlString :: Parser String
sqlString = between (char '\'') (char '\'') (many $ noneOf "\'")

entry :: Parser Entry
entry = Left <$> sqlString <|> Right . read <$> many1 digit

createTable :: Parser Create
createTable = do
  tableName <-
    keyword "CREATE"
      *> spaces
      *> keyword "TABLE"
      *> spaces
      *> name
  cols <- spaces *> parens (commas name)
  return $ Create tableName (pure cols)

insertTable :: Parser Insert
insertTable = do
  tableName <- keyword "INSERT" *> spaces' *> keyword "INTO" *> spaces' *> name <* spaces'
  values <- keyword "VALUES" *> spaces *> parens (commas entry)
  return $ Insert tableName (Insertion . pure $ values)

select :: Parser Query
select = do
  cols <- keyword "SELECT" *> spaces' *> (parens $ commas name)
  tableName <- spaces' *> keyword "FROM" *> spaces' *> (parens $ commas name)
  whereExp <- optionMaybe wherePart
  return $ Query tableName cols whereExp

wherePart :: Parser Expr
wherePart = spaces *> keyword "WHERE" *> spaces *> expy

less :: Parser Operator
less = Less <$ char '<'

greater :: Parser Operator
greater = Greater <$ char '<'

eq :: Parser Operator
eq = Eq <$ char '='

andd :: Parser (Expr -> Expr -> Expr)
andd = And <$ (spaces' *> keyword "AND" <* spaces')

orr :: Parser (Expr -> Expr -> Expr)
orr = Or <$ (spaces' *> keyword "OR" <* spaces')

operator :: Parser Operator
operator =
  Less <$ char '<'
    <|> Greater <$ char '>'
    <|> Eq <$ char '='

operand :: Parser Operand
operand = Left <$> name <|> Right <$> entry

factor :: Parser Expr
factor =
  Brackets <$> parens expy
    <|> Not <$> (spaces' *> keyword "NOT" <* spaces' *> expy)
    <|> opExpr

expy :: Parser Expr
expy = factor `chainl1` (try andd <|> try orr)

opExpr :: Parser Expr
opExpr = Op <$> operand <*> (spaces *> operator <* spaces) <*> operand

program :: Parser Program
program = do
  s <- many (Left <$> createTable <|> Right <$> insertTable)
  t <- select
  return $ Program s t
