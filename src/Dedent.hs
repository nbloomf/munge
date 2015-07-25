module Main where

import Data.List (lines, intersperse)

dedent :: String -> String
dedent = concat . intersperse "\n" . map foo . lines
  where
    foo (' ':cs)  = cs
    foo ('\t':cs) = cs
    foo x = x

main :: IO ()
main = interact dedent