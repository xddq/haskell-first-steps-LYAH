-- main method
module Main where

-- imports modules which are stored in /src/*
import qualified FirstSteps (fac, testPrint)
import qualified MyLib (someFunc)

-- determines that main will do IO stuff?
main :: IO ()
main = do
  -- prints hello world to stdout
  FirstSteps.testPrint
  -- prints result of factorial function for n = 3 to stdout
  print (FirstSteps.fac 3)
