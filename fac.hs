-- factorial calculation
fac :: Int -> Int
fac x
  | x == 0 = 1
  | otherwise = x * fac (x - 1)

main :: IO ()
main = print (fac 3)
