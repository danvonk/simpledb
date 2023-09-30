module Printer
  ( Grid (..),
    Content (..),
    (</>),
    (<^>),
    string,
    printGrid
  )
where

-- import qualified Data.Text as T

data Content = Empty
             | String String
             | Row [Grid]
             | Col [Grid] deriving (Show)

data Grid = Grid
  { rows :: Int, -- height of the string (could be over multiple lines)
    cols :: Int, -- length of the string
    content :: Content
  } deriving (Show)

string :: String -> Grid
string s = Grid {rows = 1, cols = length s, content = String s}

(</>) :: Grid -> Grid -> Grid
l </> r =
  Grid
    { rows = rows l + rows r,
      cols = cols l + cols r,
      content = Row [l, r]
    }

(<^>) :: Grid -> Grid -> Grid
l <^> r =
  Grid
    { rows = rows l + rows r,
      cols = cols l + cols r,
      content = Col [l, r]
    }


printGrid :: Grid -> String
printGrid (Grid _ _ (String s)) = s
printGrid (Grid _ _ Empty) = " "
printGrid (Grid _ _ (Col gs)) = foldr1 (\w s -> w <> ('\n':s)) out
  where
    out = printGrid <$> gs

printGrid (Grid _ _ (Row gs)) = unwords out
  where
    out = printGrid <$> gs
