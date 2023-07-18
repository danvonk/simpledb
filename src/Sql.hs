module Sql
  ( Program (..),
    Create (..),
    Insert (..),
    Insertion (..),
    Query (..),
    Expr (..),
    Operator (..),
    Name,
    Names,
    Entry,
    Entries,
    Operand,
  )
where

data Program = Program [Either Create Insert] Query deriving (Eq, Show)

data Create = Create Name (Maybe Names) deriving (Eq, Show)

data Insert = Insert Name Insertion deriving (Eq, Show)

data Insertion = Insertion (Maybe Entries) | InsQuery Query deriving (Eq, Show)

data Query = Query Names Names (Maybe Expr) deriving (Eq, Show)

data Expr
  = And Expr Expr
  | Or Expr Expr
  | Not Expr
  | Brackets Expr
  | Op Operand Operator Operand
  | Const Operand
  deriving (Eq, Show)

data Operator = Less | Greater | Eq deriving (Eq, Show)

type Name = String

type Names = [Name]

type Entry = Either String Int

type Entries = [Entry]

type Operand = Either Name Entry
