{-# LANGUAGE LambdaCase #-}

module Day11 where

import qualified Data.Map as Map

parse :: String -> Map.Map (Int, Int) Char
parse = Map.fromList . concat . zipWith (\y -> zipWith (\x c -> ((x, y), c)) [0..]) [0..] . lines

part1 :: IO Int
part1 = Map.size . Map.filter (== '#') . until ((==) <$> id <*> nextRound) nextRound . parse <$> readFile "input.txt"
  where
    nextRound grid = Map.mapWithKey (updatePos grid) grid
    updatePos grid (x, y) =
      let adjacents = [Map.lookup (x', y') grid | x' <- [x, x-1, x+1], y' <- [y, y-1, y+1], (x, y) /= (x', y')]
      in \case
        'L' | Just '#' `notElem` adjacents -> '#'
        '#' | (>= 4) . length $ filter (== Just '#') adjacents -> 'L'
        v -> v
