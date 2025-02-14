{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# HLINT ignore "Use isDigit" #-}

module Task2 where

import Task1 (doubleEveryOther, map, reverse, sum, toDigits) -- re-use as needed
import Prelude hiding
  ( map,
    reverse,
    sum,
  )

luhnModN :: Int -> (a -> Int) -> [a] -> Int
luhnModN base toVal xs =
  let nums = map toVal xs
      revNums = reverse nums
      doubled = doubleEveryOther revNums
      normalized = map (\x -> if x >= base then x - (base - 1) else x) doubled
      s = sum normalized
   in (base - (s `mod` base)) `mod` base

luhnDec :: [Int] -> Int
luhnDec = luhnModN 10 id

luhnHex :: [Char] -> Int
luhnHex = luhnModN 16 digitToInt

digitToInt :: Char -> Int
digitToInt c
  | c >= '0' && c <= '9' = fromEnum c - fromEnum '0'
  | c >= 'a' && c <= 'f' = 10 + (fromEnum c - fromEnum 'a')
  | c >= 'A' && c <= 'F' = 10 + (fromEnum c - fromEnum 'A')
  | otherwise = error "Non-hexadecimal digit"

validateDec :: Integer -> Bool
validateDec n =
  let ds = toDigits n
   in case ds of
        [] -> False
        _ ->
          let check = last ds
              rest = init ds
           in luhnDec rest == check

validateHex :: String -> Bool
validateHex s =
  case s of
    [] -> False
    _ ->
      let checkChar = last s
          rest = init s
          checkVal = digitToInt checkChar
       in luhnHex rest == checkVal
