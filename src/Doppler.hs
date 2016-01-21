module Main where

import Data.Char (toLower)
import System.IO
import System.Environment (getArgs)
import System.Exit (exitWith, exitSuccess, exitFailure)


-- I am feeling bored today. Let's make a utility
-- so I don't have to keep looking up ANSI color codes.

main :: IO ()
main = do
  args <- getArgs

  case args of
    {- foreground only -}
    [fg] -> do
      text <- getContents
      case colorize (fg, "null") text of
        Just x  -> putStr x
        Nothing -> showUsage >> exitFailure

    {- foreground and background -}
    [fg,bg] -> do
      text <- getContents
      case colorize (fg,bg) text of
        Just x  -> putStr x
        Nothing -> showUsage >> exitFailure

    {- ruh roh -}
    _ -> showUsage >> exitFailure

  {- reset colors and intensity -}
  putStr $ "\x1b[0;39;49m"
  exitSuccess


colorize :: (String, String) -> String -> Maybe String
colorize (fg, bg) x = do
  fore <- fgColorCode fg
  back <- bgColorCode bg
  return $ fore ++ back ++ x


-- Is this a terrible way to handle command line args,
--   or the worst way to handle command line args?

fgColorCode :: String -> Maybe String
fgColorCode x = case (map toLower x) of
  "null"         -> Just ""
  "red"          -> Just "\x1b[31m"
  "green"        -> Just "\x1b[32m"
  "yellow"       -> Just "\x1b[33m"
  "blue"         -> Just "\x1b[34m"
  "magenta"      -> Just "\x1b[35m"
  "cyan"         -> Just "\x1b[36m"
  "white"        -> Just "\x1b[37m"
  "lightred"     -> Just "\x1b[1;31m"
  "lightgreen"   -> Just "\x1b[1;32m"
  "lightyellow"  -> Just "\x1b[1;33m"
  "lightblue"    -> Just "\x1b[1;34m"
  "lightmagenta" -> Just "\x1b[1;35m"
  "lightcyan"    -> Just "\x1b[1;36m"
  "lightwhite"   -> Just "\x1b[1;37m"
  _              -> Nothing


bgColorCode :: String -> Maybe String
bgColorCode x = case (map toLower x) of
  "null"         -> Just ""
  "red"          -> Just "\x1b[41m"
  "green"        -> Just "\x1b[42m"
  "yellow"       -> Just "\x1b[43m"
  "blue"         -> Just "\x1b[44m"
  "magenta"      -> Just "\x1b[45m"
  "cyan"         -> Just "\x1b[46m"
  "white"        -> Just "\x1b[47m"
  "lightred"     -> Just "\x1b[1;41m"
  "lightgreen"   -> Just "\x1b[1;42m"
  "lightyellow"  -> Just "\x1b[1;43m"
  "lightblue"    -> Just "\x1b[1;44m"
  "lightmagenta" -> Just "\x1b[1;45m"
  "lightcyan"    -> Just "\x1b[1;46m"
  "lightwhite"   -> Just "\x1b[1;47m"
  _              -> Nothing


showUsage :: IO ()
showUsage = do
  putStr $ unlines
    [ "doppler colors stdin with ANSI (A doppler-shifted echo)"
    , "  USAGE: doppler FGCOLOR [BGCOLOR]"
    , "    colors are red, green, yellow, blue,"
    , "    magenta, cyan, white, and light colors."
    , "  Examples:"
    , "    echo \"wut\" | doppler red"
    , "    echo \"hello\" | doppler lightgreen cyan"
    ]
