module Main where

import Data.List.Split
import qualified Data.Map.Strict as M

linePoints ((x1, y1), (x2, y2)) = zip xRange yRange
  where
    sizeX = abs (x1 - x2) + 1
    sizeY = abs (y1 - y2) + 1
    xRange =
      if sizeX == 1
        then replicate sizeY x1
        else
          if x1 > x2
            then [x1, pred x1 .. x2]
            else [x1 .. x2]
    yRange =
      if sizeY == 1
        then replicate sizeX y1
        else
          if y1 > y2
            then [y1, pred y1 .. y2]
            else [y1 .. y2]

insertLine line grid = foldr incPoint grid points
  where
    points = linePoints line
    incPoint coord = M.insertWith (+) coord 1

parse = map (parseLine . map (map read . splitOn ",") . splitOn " -> ") . lines
  where
    parseLine [[x1, y1], [x2, y2]] = ((x1, y1), (x2, y2))

isStraight ((x1, y1), (x2, y2)) = x1 == x2 || y1 == y2

isValidDiagonal ((x1, y1), (x2, y2)) = abs (x1 - x2) == abs (y1 - y2)

solve criteria = M.size . M.filter (>= 2) . foldr insertLine M.empty . filter criteria . parse

part1 = solve isStraight

part2 = solve ((||) <$> isValidDiagonal <*> isStraight)

main :: IO ()
main = do
  input <- readFile "input.txt"

  putStrLn ("Part 1: " ++ show (part1 input))
  putStrLn ("Part 2: " ++ show (part2 input))
