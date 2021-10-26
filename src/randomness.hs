-- src: http://learnyouahaskell.com/input-and-output
-- randomness

import qualified System.Random as Random
import qualified Data.Char as Char

-- since haskell is a pure language with pure functions (same input --> same
-- output) it is impossible to create a function that returns a random number.
-- --> in haskell we create a function that takes randomness and produces an
-- output based on it :D
-- randomness --> System.Random
-- type StdGen exportet from System.Random enables us to create that randomness.
-- mkStdGen :: Int -> StdGen --> takes an integer and returns a randomness
-- generator based on the number.
-- NOTE: add random into your .cabal file and run 'cabal install random' to have
-- random library in ghci as well.

-- some ghci stuff:
-- Prelude System.Random> :t random
-- random :: (Random a, RandomGen g) => g -> (a, g)
-- Prelude System.Random> random (mkStdGen 100)
-- (9216477508314497915,StdGen {unStdGen = SMGen 712633246999323047 2532601429470541125})
-- Prelude System.Random> random (mkStdGen 100) :: (Int, StdGen)
-- (9216477508314497915,StdGen {unStdGen = SMGen 712633246999323047 2532601429470541125})
-- Prelude System.Random> random (mkStdGen 100) :: (Float, StdGen)

-- throw a coin randomly three times based on a given random generator.
threeCoins :: Random.StdGen -> (Bool, Bool, Bool)
threeCoins gen =
    let (firstCoin, newGen) = Random.random gen
        (secondCoin, newGen') = Random.random newGen
        (thirdCoin, newGen'') = Random.random newGen'
    in (firstCoin, secondCoin, thirdCoin)
-- ghci:
-- *Main> threeCoins (Random.mkStdGen 20)
-- (True,True,True)
-- *Main> threeCoins (Random.mkStdGen 21)
-- (True,False,False)


-- there is a function for the implementation we just created above. It is
-- called randoms. It takes a StdGen and produces an infinite list of random
-- numbers.
-- fiveRandomNumbers :: [Int]
-- fiveRandomNumbers = take 5 $ Random.randoms $ Random.mkStdGen 10
-- fiveRandomNumbers' :: [Int]
-- fiveRandomNumbers' = take 5 $ Random.randoms $ Random.mkStdGen 11
-- random numbers within specifc range --> Random.randomR
-- randomR :: (RandomGen g, Random a) :: (a, a) -> g -> (a, g)
-- randomR (1,6) (mkStdGen 10)

-- so far all random generation has been based upon fixed numbers --> will
-- always return in same result...
-- To fix this issue we can pull a random generator source from our OS and use
-- this as random generator. --> function getStdGen

main = do
    gen <- Random.getStdGen
    -- randomRs --> same as randomR, but returns list of random values instead
    -- of a tuple with a random number and a new generator.
    putStrLn $ take 20 (Random.randomRs ('a','z') gen)
    -- NOTE(pierre): with getStdGen we only get a unique random generator. If we
    -- want to create "different" random output based on it we have to partition
    -- the result of our random generation.
    -- e.g.
    let randomChars = Random.randomRs ('a','z') gen
        (first20, rest) = splitAt 20 randomChars
        (second20, _) = splitAt 20 rest
    -- NOTE(pierre). we can also use newStdGen which basicly does the same.
    -- Splits current stdGen, encapsulates one of them and takes the other one
    -- as new stdGen. --> after calling newStdGen, getStdGen also returns a
    -- different stdGen because of this.
    putStrLn $ "first 20: " ++ first20
    putStrLn $ "second 20: " ++ second20
    guessNumber gen

-- asks user for a number and compares it with a generated number based on the
-- given random generator. loops recursively forever.
guessNumber :: Random.StdGen -> IO ()
guessNumber gen = do
    putStrLn "Which number am I thinking of? (1 to 10)"
    -- TODO(pierre): why cant we use getChar here?
    -- numberAsChar <- getChar
    numberAsString <- getLine
    -- have to give type definition to have haskell infer correct types for
    -- randomNumber (must be Int since it is compared to number)
    let number = read numberAsString :: Int
    let (randomNumber, newGen) = Random.randomR (1,10) gen
    if number == randomNumber
            then putStrLn "Correct!"
            else putStrLn $ "Sorry, I thought of number: " ++ show randomNumber
    guessNumber newGen

-- another possible solution, using newStdGen instead.
-- (previous version prefered)
-- main = do
--     gen <- getStdGen
--     let (randNumber, _) = randomR (1,10) gen :: (Int, StdGen)
--     putStr "Which number in the range from 1 to 10 am I thinking of? "
--     numberString <- getLine
--     when (not $ null numberString) $ do
--         let number = read numberString
--         if randNumber == number
--             then putStrLn "You are correct!"
--             else putStrLn $ "Sorry, it was " ++ show randNumber
--         newStdGen
--         main


--- CONTINUE at bytestrings.hs
