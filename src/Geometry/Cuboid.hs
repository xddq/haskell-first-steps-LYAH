module Geometry.Cuboid
  ( area
  , volume
  ) where

rectangleArea :: Float -> Float -> Float
rectangleArea x y = x * y

-- cuboid="Quader"
area :: Float -> Float -> Float -> Float
area x y z =
  sum $ map (* 2) [rectangleArea x y, rectangleArea y z, rectangleArea x z]

-- NOTE: only do volume for one sample.
volume :: Float -> Float -> Float -> Float
volume a b c = a * b * c
