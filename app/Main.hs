-- main method
module Main where

-- imports modules which are stored in /src/*
import qualified Hello (testPrint)
import qualified MyLib (someFunc)

-- determines that main will do IO stuff?
main :: IO ()
main = do
  Hello.testPrint
  putStrLn "Hello, Haskell!"
  MyLib.someFunc
