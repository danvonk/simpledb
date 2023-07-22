-- |

module Table where


import qualified Data.Map as M
import Data.Map (Map (..))

type DB = Map TName Table
type TName = String

data Table = Table Columns [Row] deriving (Show)
type Columns = [Name]
type Name = String
type Row = [Entry]

data Entry = String String | Int Int deriving (Show, Eq, Ord)

exampleTable :: Table
exampleTable =
  Table  ["course",          "year",   "lecturer"]
        [[String "INFOAFP",  Int 2007, String "Andres Loeh"],
         [String "INFOAFP",  Int 2006, String "Bastiaan Heeren"],
         [String "INFOFPLC", Int 2008, String "Andres Loeh"],
         [String "INFOSWE",  Int 2008, String "Jurriaan Hage"]]
