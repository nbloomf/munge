module Main where

import Data.List (lines, intersperse)

indent :: String -> String
indent = concat . intersperse "\n" . map (' ':) . lines

main :: IO ()
main = interact indent