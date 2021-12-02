module Main where

solve :: [Int] -> Int
solve = snd . foldl f (-1, 0)
  where
    f (prev, total) actual
      | prev == -1 = (actual, total)
      | actual > prev = (actual, total + 1)
      | otherwise = (actual, total)

measurements :: [Int] -> [[Int]]
measurements xs = take (length xs - 2) $ zipWith (\n -> take 3 . drop n) [0 ..] $ replicate (length xs) xs

main :: IO ()
main = do
  input <- map read . lines <$> readFile "input.txt"

  let part1 = solve input
  putStrLn ("Part 1: " ++ show part1)

  let part2 = solve $ map sum $ measurements input
  putStrLn ("Part 2: " ++ show part2)
