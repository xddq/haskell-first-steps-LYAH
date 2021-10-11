-- main method
module Main where

-- imports modules which are stored in /src/*
import qualified FirstSteps (add, evens, fac, testPrint)
import qualified Geometry.Cuboid as Cuboid
import qualified Prelude

-- IO determines that main will do impure stuff?
main :: Prelude.IO ()
main = do
  Prelude.putStrLn "Hello, world!"
  -- prints hello world to stdout
  FirstSteps.testPrint
  -- prints result of factorial function for n = 3 to stdout
  Prelude.print (FirstSteps.fac 3)
  -- prints even numbers from given list
  Prelude.print (FirstSteps.evens [1, 2, 3, 4, 5, 6, 7])
  -- let declares variable in local scope. looks like it is acessable in whole
  -- do block
  -- in haskell it seems like functions are curried by default
  let plusTwo = FirstSteps.add 2
  Prelude.print (plusTwo 3)
  -- try out Cuboid submodules of own Geometry module.
  Prelude.putStrLn "calculating area and volume of cuboid with values: 3 4 5"
  Prelude.putStrLn ("volume: " Prelude.++ Prelude.show (Cuboid.volume 3 4 5))
  Prelude.putStrLn ("area: " Prelude.++ Prelude.show (Cuboid.area 3 4 5))
