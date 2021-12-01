{-# LANGUAGE LambdaCase #-}

module Day10 where

import Data.List

diff :: Int -> [Int] -> [Int]
diff _ [] = [3]
diff prev (current:rest) = (current - prev) : diff current rest

solve :: ([Int] -> Int) -> IO Int
solve f = f . diff 0 . sort . map read . lines <$> readFile "input.txt"

part1 :: IO Int
part1 = solve (product . map length . group . sort)

part2 :: IO Int
part2 = solve (foldr f 1 . group)
  where
    f = \case
      [1, 1]       -> (2*)
      [1, 1, 1]    -> (4*)
      [1, 1, 1, 1] -> (7*)
      _            -> id
