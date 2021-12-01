module Day06 where

import Data.List.Split
import Data.Set hiding (map, foldr)

type Answers = Set Char
type Group = [Answers]

solve :: (Group -> Answers) -> IO Int
solve f = sum . map (size . f . map fromList . lines) . splitOn "\n\n" <$> readFile "input.txt"

part1 :: IO Int
part1 = solve unions

part2 :: IO Int
part2 = solve (foldr1 intersection)
