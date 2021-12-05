{-# LANGUAGE TupleSections #-}

module Main where

import Data.List
import Data.List.Split

type Board = [[(Int, Bool)]]

draw :: Int -> Board -> Board
draw n = (map . map) check
  where
    check (x, marked)
      | x == n = (x, True)
      | otherwise = (x, marked)

isWinner :: Board -> Bool
isWinner board = any and rows || any and cols
  where
    rows = (map . map) snd board
    cols = transpose rows

parse input = (ns, boards)
  where
    (nsStr : boardsStrs) = splitOn "\n\n" input
    ns = map read $ splitOn "," nsStr
    boards = map (map (map ((,False) . read) . words) . lines) boardsStrs

score (n, board) = unmarkedSum * n
  where
    unmarkedSum = sum $ concatMap (map fst . filter (not . snd)) board

part1 :: String -> Int
part1 = score . play . parse
  where
    play (n : ns, boards) =
      let boards' = map (draw n) boards
          winner = find isWinner boards'
       in case winner of
            Nothing -> play (ns, boards')
            Just board -> (n, board)

part2 :: String -> Int
part2 = score . play (0, []) . parse
  where
    play lastWin (n : ns, boards) =
      let boards' = map (draw n) boards
          winner = find isWinner boards'
          losers = filter (not . isWinner) boards'
          lastWin' = case winner of
            Nothing -> lastWin
            Just board -> (n, board)
       in case losers of
            [] -> lastWin'
            xs -> play lastWin' (ns, xs)

main :: IO ()
main = do
  input <- readFile "input.txt"

  putStrLn ("Part 1: " ++ show (part1 input))
  putStrLn ("Part 1: " ++ show (part2 input))
