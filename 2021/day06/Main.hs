{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Arrow
import Data.List
import Data.List.Split
import qualified Data.Map as M

part1 = length . (!! 80) . iterate (concatMap tick) . map read . splitOn ","
  where
    tick = \case
      0 -> [6, 8]
      n -> [n - 1]

part2 = iter 256 . parse
  where
    parse = M.fromList . map (head &&& length) . group . sort . map read . splitOn ","

    inc = M.insertWith (+)
    dec = M.insertWith (flip (-))

    iter 0 fishes = sum (M.elems fishes)
    iter n fishes = iter (n - 1) (M.foldlWithKey tick fishes fishes)

    tick fishes 0 v = case M.lookup 0 fishes of
      Just n | n > 0 -> inc 8 n $ inc 6 n $ dec 0 n fishes
      _ -> fishes
    tick fishes k v = inc (k - 1) v $ dec k v fishes

main :: IO ()
main = do
  input <- readFile "input.txt"

  putStrLn ("Part 1: " ++ show (part1 input))
  putStrLn ("Part 2: " ++ show (part2 input))
