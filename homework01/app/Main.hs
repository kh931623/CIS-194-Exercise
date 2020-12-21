module Main where

import Lib

main :: IO ()
main = someFunc

toDigits :: Integer -> [Integer]
toDigits n
  | n < 0 = []
  | n == 0 = []
  | otherwise = toDigits divided ++ [remaining]
  where
    remaining = mod n 10
    divided = div n 10

toDigitRev :: Integer -> [Integer]
toDigitRev n
  | n <= 0 = []
  | otherwise = remaining : toDigitRev divided
  where
    remaining = mod n 10
    divided = div n 10

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther [x, y] = [x * 2, y]
doubleEveryOther (x : y : xs)
  | isEven = [x * 2, y] ++ doubleEveryOther xs
  | otherwise = [x, y * 2] ++ doubleEveryOther xs
  where
    isEven = even (length xs)

sumDigits :: [Integer] -> Integer
sumDigits = foldl (\acc x -> mod x 10 + div x 10 + acc) 0

validate :: Integer -> Bool
validate 0 = False
validate n = (sumDigits . doubleEveryOther . toDigits) n `mod` 10 == 0

type Peg = String

type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi 1 a b _ = [(a, b)]
hanoi n a b c = hanoi (n -1) a c b ++ [(a, b)] ++ hanoi (n -1) c b a