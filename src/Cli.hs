-- from "Learn You a Haskell"
-- http://learnyouahaskell.com/input-and-output

import Data.List
import System.Environment

main = do
  args <- getArgs -- IO [String]
  progName <- getProgName -- IO String
  putStrLn "The arguments are:"
  mapM_ putStrLn args
  putStrLn "The program name is:"
  putStrLn progName
