-- main method
module Main where

-- imports modules which are stored in /src/*
import qualified FirstSteps (add, evens, fac, testPrint)
import qualified MyLib (someFunc)

-- determines that main will do IO stuff?
main :: IO ()
main = do
  -- prints hello world to stdout
  FirstSteps.testPrint
  -- prints result of factorial function for n = 3 to stdout
  print (FirstSteps.fac 3)
  -- prints even numbers from given list
  print (FirstSteps.evens [1, 2, 3, 4, 5, 6, 7])
  -- let declares variable in local scope. looks like it is acessable in whole
  -- do block
  -- in haskell it seems like functions are curried by default
  let plusTwo = FirstSteps.add (2)
  print (plusTwo 3)
  print (plusTwo 4)

-- TODO(pierre):
-- 1) Where can I actually find decent docu on haskell? Can't
-- even find something on https://www.haskell.org/documentation/ ??? Somewhere
-- all the types etc. must be defined, no?
-- 2) Why is coc with language server for haskell not working? E.g. I can't go
-- to definition for "print" or "do" or "IO"
