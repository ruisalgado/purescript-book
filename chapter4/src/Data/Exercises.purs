module Data.Exercises where

import Data.Array.Unsafe (head, tail)
import Data.Array (filter, range, length, (..), map)
import Control.MonadPlus (guard)
import Data.Foldable (any, foldl)

isEven :: Number -> Boolean
isEven 0 = true
isEven 1 = false
isEven nr = isEven (nr % 2)

countEven :: [Number] -> Number
countEven [] = 0
countEven arr = (if isEven x then 1 else 0) + countEven xs
  where
  x = head arr
  xs = tail arr

squares :: [Number] -> [Number]
squares arr = square <$> arr
  where
  square n = n * n

(<$?>) = filter

positives :: [Number] -> [Number]
positives arr = (\n -> n >= 0) <$?> arr

factors :: Number -> [[Number]]
factors n = do
  i <- range 1 n
  j <- range i n
  guard $ i * j == n
  return [i, j]

isPrime :: Number -> Boolean
isPrime 1 = false
isPrime n = length (factors n) == 1

cartesianProduct :: [Number] -> [Number] -> [[Number]]
cartesianProduct a b = do
  ia <- a
  ib <- b
  return [ia, ib]

triples :: Number -> [[Number]]
triples n = do
  a <- 3 .. n
  b <- a .. n
  c <- b .. n
  guard $ a * a + b * b == c * c
  return [a, b, c]

allTrue :: [Boolean] -> Boolean
allTrue arr = foldl (\acc val -> acc && val) true arr

count :: forall a. (a -> Boolean) -> [a] -> Number
count pred = count' 0
  where
  count' acc [] = acc
  count' acc (x : xs) = count' (if pred x then acc + 1 else acc) xs

reverse :: forall a. [a] -> [a]
reverse = foldl (\acc x -> x : acc) []
