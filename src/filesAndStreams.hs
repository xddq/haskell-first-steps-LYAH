-- files and streams
-- src: http://learnyouahaskell.com/input-and-output
-- getContents --> stdin, reads everything until it receives and EOF(end-of-file)
-- character.
--
import qualified Data.Char as Char
import qualified System.IO as IO

-- displays piped input in full caps.
-- 1) ghc --make filesAndStreams.hs
-- 2) echo "hello world" | ./filesAndStreams
-- NOTE(pierre): Why does it load contents line by line instead of full text and
-- then print stuff? Why not char by char?
-- --> does always load after we input a newline character?
-- main = do
--   pipedText <- getContents
--   putStrLn "Your input in full caps: "
--   putStr $ map Char.toUpper pipedText

-- pipe and print only lines that are length <10
-- 1) ghc --make filesAndStreams.hs
-- 2) echo "hello world" | ./filesAndStreams
-- 3) cat testfile | ./filesAndStreams
-- main = do
--     pipedText <- getContents
--     putStrLn "All lines with <10 characters"
--     let allLines = lines pipedText
--     putStr $ unlines $ filter (\line -> length line < 10) allLines

-- INTERACT --> function for commonly used workflow: getting input from stdin,
-- transforming it with a function and returning output.
-- pipe and print only lines that are length <10, using interact.
-- main = do
--     putStr "All lines with <10 characters"
--     let allLines = lines
--     interact (\pipedText -> unlines . filter (\line -> length line < 10) $ allLines pipedText)

-- (with solution)
-- pipe and print only lines that are length <10, using interact.
-- main = do
--     putStr "All lines with <10 characters"
---- interact $ unlines . filter (\line -> length line < 10) . lines
---- NOTE(pierre): check pint free with filter, length again. when can we use
---- point free? --> I think, if we partially apply and don't write the 'last'
---- argument?
-- interact $ unlines . filter ((<10) . length) . lines

-- prints palindrom or no palindrom based on the check if the given line is a
-- palindrom.
-- main = do
--     -- MAYBE(pierre): why does this output not print out instantly when we run
--     -- the program? Somehow only after we give the first input???
--     putStr "Prints 'palindrom' or 'no palindrom' for given line."
--     let isPalindrom xs = xs == reverse xs
--     interact $ unlines . map (\line -> if isPalindrom line then "palindrom" else "no palindrom") . lines

-- FILES.
-- hGetContents -> gets all contents from handle (handle is a pointer to a
-- file). reads until EOF character.
-- openFile -> generates handle for given path
-- use :info IOMode to get the possible value constructors for IOMode type.
-- main = do
--     -- NOTE(pierre): after openFile we have to close the file/handle by
--     -- ourselves!
--     handle <- IO.openFile "./testfile" IO.ReadMode
--     text <- IO.hGetContents handle
--     putStr "The given file content was: "
--     putStr text
--     IO.hClose handle

-- since this is typical/often/commonly used behaviour, we have a functions for
-- this. withFile.
-- :t withFile
-- withFile :: FilePath -> IOMode -> (Handle -> IO r) -> IO r
-- withFile takes a filePath, an IO mode, a function that takes a handle and
-- returns an IO action and a value. returns the return value from given
-- function.
-- main = do
-- --     -- TODO(pierre): why can we not assign the result from IO.withFile to a variable?
-- --     -- result <- IO.withFile "./testfile" IO.ReadMode (\handle -> do
-- --     --                                     text <- IO.hGetContents handle
-- --     --                                     putStrLn "The given file content was: "
-- --     --                                     putStr text)
-- --     -- return result
--     -- withFile "./testfile" IO.ReadMode (\handle -> do
--     --                                     text <- IO.hGetContents handle
--     --                                     putStrLn "The given file content was: "
--     --                                     putStr text)
--     withFile' "./testfile" IO.ReadMode (\handle -> do
--                                         text <- IO.hGetContents handle
--                                         putStrLn "The given file content was: "
--                                         putStr text)

-- writing own withFile
withFile' path mode f = do
  handle <- IO.openFile path mode
  result <- f handle
  IO.hClose handle
  return result

-- FILE operations are similar to 'normal' io operations. e.g. getChar, putStr,
-- putStrLn, etc..
-- how can we read chars from file?
-- - hGetChar
-- how can we read a line from a file?
-- - hGetLine
-- how can we write string until EOF or a line to file?
-- - hPutStr; hPutStrLn

-- we can use readFile to open a file and read out its contents in one
-- step/function. file will be automaticly closed after having read stuff.
-- main = do
--     text <- readFile "./testfile"
--     putStrLn "the text we got was: "
--     putStr text

-- writeFile --> similar to readFile. Careful. Opens file in read mode (does not
-- append, rather overwrite file from scratch!)
-- appendFile --> similar to writeFile, but does append content :]
-- main = do
--     text <- readFile "./testfile"
--     putStrLn "going to read in ./testfile and write the uppercased text to ./testwritefile using writeFile which does overwrite an existing file."
--     writeFile "./testwritefile" $ map Char.toUpper text
main = do
  text <- readFile "./testfile"
  putStrLn "going to read in ./testfile and write the uppercased text to ./testwritefile using appendFile which does append content to the existing file."
  appendFile "./testwritefile" $ map Char.toUpper text

-- TODO(pierre): continue at todo.txt
