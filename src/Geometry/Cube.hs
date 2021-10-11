module Geometry.Cube
  ( area
  , volume
  ) where

import qualified Geometry.Cuboid as Cuboid

-- NOTE: using Maybe to handle error case output.
volume :: Float -> Float -> Float -> Maybe Float
volume x y z =
  if any (/= x) [x, y, z]
    then Nothing
    else Just (Cuboid.volume x y z)

area :: Float -> Float -> Float -> Maybe Float
area x y z =
  if any (/= x) [x, y, z]
    then Nothing
    else Just (Cuboid.area x y z)
