module Data.Exercises where

import Data.Picture
import Data.Maybe

allTrue :: [Boolean] -> Boolean
allTrue [] = true
allTrue (x : xs) = x && allTrue xs

isSorted :: [Number] -> Boolean
isSorted (n : m : ns) | n > m = false
isSorted (n : m : ns)         = isSorted ns
isSorted _                    = true

getCity :: forall a b. { address :: { city :: String | b } | a } -> String
getCity person = person.address.city

flatten :: forall a. [[a]] -> [a]
flatten [] = []
flatten [x] = x
flatten (x : y : xs) = x ++ y ++ flatten xs

scale :: Shape -> Shape
scale (Circle p r) = Circle p (r * 2)
scale (Rectangle p w h) = Rectangle p (w * 2) (h * 2)
scale (Line (Point a) (Point b)) = Line (Point offsetA) (Point offsetB)
  where
  offset = { x: (b.x - a.x) / 2, y: (b.y - a.y) / 2 }
  offsetA = { x: a.x - offset.x, y: a.y - offset.y }
  offsetB = { x: b.x + offset.x, y: b.y + offset.y }
scale t@(Text _ _) = t

extractText :: Shape -> Maybe String
extractText (Text _ t) = Just t
extractText _ = Nothing
