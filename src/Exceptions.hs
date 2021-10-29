-- can use Maybe and Either type for stuff that may or may not be there.
-- only use exceptions in the IO/side effects/impure part of your code.
-- in pure code always use maybe or either to handle stuff that may have bad
-- results.

-- try catch similar to java, python.

import GHC.IO
import System.Environment
import qualified System.IO.Error as SysIOErr

-- main = toTry `catch` handler
--
-- -- can also use catch toTry handler
-- -- `name` uses infix notation.
--
-- toTry :: IO ()
-- toTry = do
--   (fileName : _) <- getArgs
--   contents <- readFile fileName
--   putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"
--
-- handler :: IOError -> IO ()
-- handler e = putStrLn "Whoops, had some trouble!"

-- rewrite and handle exception that we want to catch. if we have any other,
-- throw it to someone else..
main = toTry `catch` handler

-- can also use catch toTry handler
-- `name` uses infix notation.

toTry :: IO ()
toTry = do
  (fileName : _) <- getArgs
  contents <- readFile fileName
  putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"

handler :: IOError -> IO ()
handler e
  -- had to go through real pain by browsing: For example I want to know what
  -- can happen for readFile.  I searched hoogle for readFile -->
  -- https://hackage.haskell.org/package/base-4.16.0.0/docs/Prelude.html#v:readFile
  -- Then saw it uses openFile -->
  -- https://hackage.haskell.org/package/base-4.16.0.0/docs/src/System.IO.html#readFile
  -- And here I am seeing POSIX.openFile -->
  -- https://hackage.haskell.org/package/base-4.16.0.0/docs/src/GHC.IO.StdHandles.html#openFile
  -- with result:
  -- This operation may fail with:
  --
  --  * 'System.IO.Error.isAlreadyInUseError' if the file is already open and
  --    cannot be reopened;
  --
  --  * 'System.IO.Error.isDoesNotExistError' if the file does not exist or
  --    (on POSIX systems) is a FIFO without a reader and 'WriteMode' was
  --    requested; or
  --
  --  * 'System.IO.Error.isPermissionError' if the user does not have permission
  --     to open the file.
  -- SOLUTION: there are all errors defiend here: https://downloads.haskell.org/~ghc/6.10.1/docs/html/libraries/base/System-IO-Error.html#3
  -- note this is ghc 6.10 version stuff..
  | SysIOErr.isDoesNotExistError e = putStrLn "File does not exist!"
  | SysIOErr.isFullError e = freeSomeSpace
  | SysIOErr.isIllegalOperation e = notifyCops
  | otherwise = ioError e

freeSomeSpace = putStrLn "free some space!"
notifyCops = putStrLn "illegal action.. calling the cops!"

-- some predicates for io error types:
-- isAlreadyExistsError
-- isDoesNotExistError
-- isAlreadyInUseError
-- isFullError
-- isEOFError
-- isIllegalOperation
-- isPermissionError
-- isUserError

