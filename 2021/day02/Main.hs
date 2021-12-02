{-# LANGUAGE LambdaCase #-}

module Main where

import Data.Char (isSpace)
import Data.List (isPrefixOf)

data Move
  = Up Int
  | Down Int
  | Forward Int
  deriving (Show)

parse :: String -> Move
parse str = move count
  where
    (moveStr, ' ' : countStr) = break isSpace str
    count = read countStr
    move = case moveStr of
      "up" -> Up
      "down" -> Down
      "forward" -> Forward
      _ -> undefined

part1 :: [Move] -> Int
part1 = uncurry (*) . foldl step (0, 0)
  where
    step (x, y) = \case
      Up n -> (x, y - n)
      Down n -> (x, y + n)
      Forward n -> (x + n, y)

part2 :: [Move] -> Int
part2 = (\(x, y, _) -> x * y) . foldl step (0, 0, 0)
  where
    step (x, y, aim) = \case
      Up n -> (x, y, aim - n)
      Down n -> (x, y, aim + n)
      Forward n -> (x + n, y + (n * aim), aim)

main :: IO ()
main = do
  input <- map parse . lines <$> readFile "input.txt"

  putStrLn ("Part 1: " ++ show (part1 input))
  putStrLn ("Part 2: " ++ show (part2 input))
