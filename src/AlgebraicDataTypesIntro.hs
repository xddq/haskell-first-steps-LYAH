-- http://learnyouahaskell.com/making-our-own-types-and-typeclasses
-- How Bool is actually defined: (somehow the source at
-- https://hackage.haskell.org/package/base-4.15.0.0/ghc-prim-0.7.0/src/GHC-Types.html#Bool
-- can't be inspected??)

-- NOTE: dataType(..) -> exports all value constructors for dataType.
-- dataType(Circle) -> exports only value constructor Circle for dataType.
-- Here we don't export value constructors for Points at all! We are still able
-- to create Points using the auxilliary="hilfs" function basePoint.
module AlgebraicDataTypesIntro
  ( Shape (..),
    area,
    nudge,
    basePoint,
    baseCircle,
  )
where

-- 1) When we have types that are no templates / take no variables we use data to
-- make it explicit, that the type does not take variables.
-- e.g.
-- data Bool = False | True
data MyBool = False | True

-- MAYBE(pierre): Why do I need to use Main.True? What does 'Main.' refer to?
-- Definitions made in the current file?
-- SOLVED(pierre): -> Default Module name will be Main until we write:
-- 'module nameHere ( ...exports....) where'
checkTrue :: (Eq a, Num a) => a -> MyBool
-- why did this not work?
-- checkTrue a = a == 1
checkTrue a = if a == 1 then AlgebraicDataTypesIntro.True else AlgebraicDataTypesIntro.False

-- define shapes circle and rectangle
-- circle expects two points as middle point of the circle and third value as
-- radius.
-- rectangle expets 2 points. Deriving from Show so we can print it to console.
data Point = Point Float Float deriving (Show)

-- NOTE(pierre): why does it tell me to use "newtype" instead of Radius for less
-- laziness?
data Radius = Radius Float deriving (Show)

data Shape = Circle Point Radius | Rectangle Point Point deriving (Show)

-- calculate area for different shapes
area :: Shape -> Float
area (Circle _ (Radius radius)) = pi * radius ^ 2
area (Rectangle (Point x1 y1) (Point x2 y2)) = product . map abs $ [x1 - x2, y1 - y2]

-- testing area function
circleArea = area (Circle (Point 0 0) (Radius 24))

rectangleArea = area (Rectangle (Point 0 0) (Point 100 100))

-- nudging circle or rectangle by x and y amount
nudge :: Shape -> Float -> Float -> Shape
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b = Rectangle (Point (x1 + a) (y1 + b)) (Point (x2 + a) (y2 + b))
nudge (Circle (Point x1 y1) (Radius radius)) a b = Circle (Point (x1 + a) (y1 + b)) (Radius radius)

-- can create auxiliary functions which will make our live easier. E.g. we don't
-- want to create points.
basePoint :: Point
basePoint = Point 0 0

-- doesn't help us too much. Lets say we don't want to create points at all.
baseCircle :: Radius -> Shape
baseCircle (Radius radius) = Circle basePoint (Radius radius)

testBasePoint = basePoint

-- testing baseCircle and basePoint function.
-- point-free-style notation. Could also use:
-- testBaseCircle radius = baseCircle (Radius radius)
testBaseCircle = baseCircle . Radius
