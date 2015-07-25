module Main where

import Data.List (lines, unlines, sort, group)
import System.IO
import System.Environment (getArgs)
import System.Console.GetOpt (getOpt, ArgOrder(Permute), OptDescr(Option), ArgDescr(ReqArg, NoArg), usageInfo)
import System.Exit (exitWith, ExitCode(ExitSuccess, ExitFailure))

tally :: (Eq a, Ord a) => [a] -> [(a, Integer)]
tally = map count . group . sort
  where
    count [] = error "in tally: empty list"
    count xs = (head xs, sum [1 | x <- xs])

freq :: String -> String
freq str = unlines $ map pretty $ tally $ lines str
  where
    pretty (x,n) = x ++ " " ++ show n
        

main = do
  args               <- getArgs
  (actions, _, errs) <- return $ getOpt Permute options args
  opts               <- foldl (>>=) (return defaultArgs) actions

  {---------------------------------------------------}
  {- If any command line errors, show usage and quit -}
  {---------------------------------------------------}
  _ <- if errs == []
        then return ()
        else showErrors errs >> showUsage >> exitWith (ExitFailure 1)

  {-----------------------------------------}
  {- If --help is set, show usage and quit -}
  {-----------------------------------------}
  _ <- if (helpFlag opts) == True
        then showUsage >> exitWith ExitSuccess
        else return ()

  f <- if (dataFlag opts == True)
        then readFile (dataPath opts)
        else getContents

  putStr $ freq f
  exitWith ExitSuccess

showUsage     = hPutStrLn stdout (usageInfo "Usage: a [OPTION...]" options)
showErrors es = hPutStrLn stderr (concat es)

data Flag = Flag
 { binsFlag :: Bool,   binsString :: String
 , dataFlag :: Bool, dataPath :: String
 , boundsFlag :: Bool, boundsString :: String
 , helpFlag :: Bool
 } deriving (Eq, Show)

defaultArgs = Flag
 { binsFlag = False, binsString = "1"
 , dataFlag = False, dataPath = ""
 , boundsFlag = False, boundsString = "0"
 , helpFlag = False
 }

options :: [OptDescr (Flag -> IO Flag)]
options = 
 [ Option [] ["help"]
     (NoArg (\opt -> return $ opt {helpFlag = True}))
     "show usage"

 , Option ['d'] ["data"]
     (ReqArg (\arg opt -> return $ opt {dataFlag = True, dataPath = arg}) "file")
     "data file"

 , Option ['n'] ["bins"]
     (ReqArg (\arg opt -> return $ opt {binsFlag = True, binsString = arg}) "INTEGER")
     "number of bins"

 , Option ['b'] ["bounds"]
     (ReqArg (\arg opt -> return $ opt {boundsFlag = True, boundsString = arg}) "STRING")
     "upper bounds of bins"
 ]