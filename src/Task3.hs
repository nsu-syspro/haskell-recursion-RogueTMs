{-# OPTIONS_GHC -Wall #-}

module Task3 where

type Peg = String

type Move = (Peg, Peg)

hanoi :: Int -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n fromPeg toPeg sparePeg =
  hanoi (n - 1) fromPeg sparePeg toPeg
    ++ [(fromPeg, toPeg)]
    ++ hanoi (n - 1) sparePeg toPeg fromPeg
