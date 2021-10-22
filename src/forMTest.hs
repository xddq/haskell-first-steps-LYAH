-- try to better understand the example from
-- src: http://learnyouahaskell.com/input-and-output
-- at forM

-- code own forM'
forM' xs = sequence . flip map xs

testForForM = forM' [1, 2, 3] print

-- code own mapM'
mapM' f = sequence . map f

testForMapM = mapM' print [1, 2, 3]

main = do
  let numbers = [1, 2, 3]
  ranks <-
    forM'
      numbers
      ( \x -> do
          putStrLn $ "Rank the number " ++ show x ++ " from 1 to 10!"
          rank <- getLine
          return rank
      )
  putStrLn $ "You gave the numbers: " ++ show numbers
  putStrLn $ "The following ranks: " ++ unwords ranks

-- could also write this to practice mapM.
-- mapM putStrLn ranks
-- TODO(pierre): how could we print "The following ranks:" with mapM?
-- mapM (\x -> putStrLn "The following ranks: " + x) ranks -- nope .. xD

-- NOTE(pierre): rank <- getLine --> unwrapping IO String to String
-- return rank --> wrapping String to IO String.
-- --> we can just write getLine instead of these two lines.

-- solution from book
-- main = do
--     colors <- forM' [1,2,3,4] (\a -> do
--         putStrLn $ "Which color do you associate with the number " ++ show a ++ "?"
--         color <- getLine
--         return color)
--     putStrLn "The colors that you associate with 1, 2, 3 and 4 are: "
--     mapM putStrLn colors
--
-- How would you explain forM? What does it do?
-- - takes a function that produces an IO action/side effect and a box/container
-- with traversable elements/supports map function. Returns container with
-- results from the functions.

-- RECAP of I/O / side effects.
-- IO actions are values, just like any other value in Haskell.
-- - IO actions can be passed to functions (e.g. mapM, forM)
-- - Functions can return IO actions. (putStrLn, return)
--
-- putStrLn does not print something to the terminal. It is a function that
-- takes a String and returns an IO action. This IO action then prints to the
-- terminal.
--
--
-- CONTINUE WITH filesAndStreams.hs
