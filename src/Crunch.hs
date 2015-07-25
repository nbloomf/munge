module Main where

crunch :: String -> String
crunch []            = []
crunch [' ']         = []
crunch ['\t']        = []
crunch [c]           = [c]
crunch (' ':' ':cs)  = crunch (' ':cs)
crunch (' ':'\t':cs) = crunch (' ':cs)
crunch (' ':'\n':cs) = '\n' : crunch cs
crunch ('\t':cs)     = crunch (' ':cs)
crunch (c:cs)        = c : crunch cs


main :: IO ()
main = interact crunch