module Main where

import Data.Function
import Data.List

binToDec :: [Bool] -> Int
binToDec = foldl (\x y -> fromEnum y + 2 * x) 0

mostCommon f n = head . f (compare `on` length) . group . sort . (!! n) . transpose

part1 :: [[Bool]] -> Int
part1 input = gammaRate input * epsilonRate input
  where
    rate f xs = binToDec $ map (\n -> mostCommon f n xs) [0 .. length (head xs) - 1]

    gammaRate = rate maximumBy
    epsilonRate = rate minimumBy

part2 :: [[Bool]] -> Int
part2 input = oxygenGeneratorRating * co2ScrubberRating
  where
    oxygenGeneratorRating = findByCriteria oxygenCriteria 0 input
    co2ScrubberRating = findByCriteria co2Criteria 0 input

    oxygenCriteria = criteria maximumBy
    co2Criteria = criteria minimumBy

    criteria f n xs = filter ((== mostCommon f n xs) . (!! n)) xs

    findByCriteria criteria n xs =
      case criteria n xs of
        [x] -> binToDec x
        xs -> findByCriteria criteria (n + 1) xs

main :: IO ()
main = do
  input <- (map . map) parse . lines <$> readFile "input.txt"

  putStrLn ("Part 1: " ++ show (part1 input))
  putStrLn ("Part 2: " ++ show (part2 input))
  where
    parse '0' = False
    parse '1' = True
    parse _ = error "invalid digit"
