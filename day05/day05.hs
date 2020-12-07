{-# LANGUAGE LambdaCase #-}

module Day05 where

import Data.List

seatIds :: IO [Int]
seatIds = map (uncurry seatId . seatPos) . lines <$> readFile "input.txt"
  where
    seatPos = ((,) <$> decode (0, 127) . fst <*> decode (0, 7) . snd) . splitAt 7
    seatId = (+) . (* 8)
    decode (lo, hi) = \case
      'F':xs -> decode (lo, avg) xs
      'B':xs -> decode (avg, hi) xs
      'L':xs -> decode (lo, avg) xs
      'R':xs -> decode (avg, hi) xs
      [] -> hi
      where avg = (lo + hi) `div` 2

part1 :: IO Int
part1 = maximum <$> seatIds

part2 :: IO Int
part2 = succ . head . head . filter ((== 2) . length) . groupBy (\a b -> b - a == 2) . sort <$> seatIds
