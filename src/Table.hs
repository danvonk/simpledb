{-# LANGUAGE OverloadedStrings #-}
module Table
  ( DB,
    Table (..),
    Entry (..),
    Row,
    Name,
    Columns,
    TName,
  )
where

import qualified Data.Text as T
import Data.Text (Text)
import Data.Map (Map (..))
import qualified Data.Map as M

type DB = Map TName Table

type TName = Text

data Table = Table Columns [Row] deriving (Show)

type Columns = [Name]

type Name = Text

type Row = [Entry]

data Entry = String Text | Int Int deriving (Show, Eq, Ord)

exampleTable :: Table
exampleTable =
  Table
    ["course", "year", "lecturer"]
    [ [String "INFOAFP", Int 2007, String "Andres Loeh"],
      [String "INFOAFP", Int 2006, String "Bastiaan Heeren"],
      [String "INFOFPLC", Int 2008, String "Andres Loeh"],
      [String "INFOSWE", Int 2008, String "Jurriaan Hage"]
    ]


rowLength :: Row -> (Int, Int, Int)
rowLength r = (head lengths, lengths !! 1, lengths !! 2)
  where
    lengths = fmap length r

printTable :: Table -> String
printTable (Table cols rows) = undefined
  where
    lengths = foldl
