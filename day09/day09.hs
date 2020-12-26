module Day09 where

import Control.Applicative

part1 :: IO Int
part1 = uncurry f . splitAt 25 . map read . lines <$> readFile "input.txt"
  where
    f preamble (n:ns)
      | n `elem` sums preamble = f (tail preamble ++ [n]) ns
      | otherwise = n
    sums xs = [x + y | x <- xs, y <- xs, x /= y]

part2 :: IO (Maybe Int)
part2 = do
  xs <- map read . lines <$> readFile "input.txt"
  invalid <- part1
  pure $ weakness invalid xs
  where
    weakness invalid (x:xs) = f invalid (x, 0, 0) (x:xs) <|> weakness invalid xs
    f _ _ [] = Nothing
    f invalid (small, largest, sum) (x:xs) =
      case compare (sum + x) invalid of
        LT -> f invalid (min x small, max x largest, sum + x) xs
        GT -> Nothing
        EQ -> Just (small + largest)
