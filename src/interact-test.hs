-- main = do
--   putStrLn "type something!"
--   something <- getLine
--   putStrLn $ "you typed: " ++ show something
--   putStrLn "type something!"
--   interact (\input -> show input)
--   return ()
--
import Control.Monad
import Data.Char

--
main = forever $ do
  putStrLn "Tell me something!"
  result <- getLine
  putStrLn $ map toUpper result

-- main = do
--     putStr "All lines with <10 characters"
--     let allLines = lines
--     interact (\pipedText -> unlines . filter (\line -> length line < 10) $ allLines pipedText)
