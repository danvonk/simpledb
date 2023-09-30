module Lib
  ( simpleDB,
  )
where

import Parser
import Printer

import Text.Parsec (parse)

simpleDB :: IO ()
simpleDB = do
  s <- getContents
  putStrLn "Parsing..."
  let p = parse program "Parse failure" s
  case p of
    Left _ -> putStrLn "parse error"
    Right _ -> putStrLn "parsed program"
