-- main method
module Main where

-- imports modules which are stored in /src/*
import qualified FirstSteps (add, evens, fac, testPrint)
import qualified MyLib (someFunc)

-- IO determines that main will do impure stuff?
main :: IO ()
main = do
  putStrLn "Hello, world!"
  -- prints hello world to stdout
  FirstSteps.testPrint
  -- prints result of factorial function for n = 3 to stdout
  print (FirstSteps.fac 3)
  -- prints even numbers from given list
  print (FirstSteps.evens [1, 2, 3, 4, 5, 6, 7])
  -- let declares variable in local scope. looks like it is acessable in whole
  -- do block
  -- in haskell it seems like functions are curried by default
  let plusTwo = FirstSteps.add 2
  print (plusTwo 3)
  print (plusTwo 4)
