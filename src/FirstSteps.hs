-- creates module for factorial calculation
module FirstSteps (fac, testPrint, evens, add) where

-- prints hello world to console
testPrint :: IO ()
testPrint = putStrLn "Hello, world!"

-- calculates factorial
fac :: Int -> Int
fac x
  | x == 0 = 1
  | otherwise = x * fac (x - 1)

-- follows stuff from youtube guide: https://www.youtube.com/watch?v=y6xiaSkVlvs
-- prints all even numbers
evens :: [Int] -> [Int]
evens [] = []
evens (x : xs)
  | even x = x : evens xs
  | otherwise = evens xs

-- adds two numbers
add :: Integer -> Integer -> Integer
add x y = x + y
