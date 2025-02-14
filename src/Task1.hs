{-# OPTIONS_GHC -Wall #-}

module Task1 where

import Prelude hiding
  ( map,
    reverse,
    sum,
  )

validate :: Integer -> Bool
validate n =
  let ds = toDigits n
   in case ds of
        [] -> False
        _ ->
          let checkDigit = last ds
              rest = init ds
           in luhn rest == checkDigit

luhn :: [Int] -> Int
luhn digits =
  let s = sum (map normalize (doubleEveryOther (reverse digits)))
   in (10 - (s `mod` 10)) `mod` 10

toDigits :: Integer -> [Int]
toDigits n
  | n <= 0 = []
  | otherwise = toDigits (n `div` 10) ++ [fromIntegral (n `mod` 10)]

reverse :: [a] -> [a]
reverse [] = []
reverse (x : xs) = reverse xs ++ [x]

doubleEveryOther :: [Int] -> [Int]
doubleEveryOther [] = []
doubleEveryOther [x] = [2 * x]
doubleEveryOther (x : y : zs) = (2 * x) : y : doubleEveryOther zs

normalize :: Int -> Int
normalize x
  | x >= 10 = x - 9
  | otherwise = x

map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x : xs) = f x : map f xs

sum :: [Int] -> Int
sum = foldr (+) 0
