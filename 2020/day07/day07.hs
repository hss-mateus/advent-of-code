module Day07 where

import Control.Applicative
import Data.List
import Data.List.Split
import Data.Char
import Data.Maybe

type BagColor = String
data Rule = Rule
  { bagColor :: BagColor
  , contents :: [(Int, BagColor)]
  }
  deriving (Eq, Show)

parse :: String -> Rule
parse entry = Rule bc contentList
  where
    [bc, cts] = splitOn " bags contain " entry
    contentList = filter ((/= "") . snd) . map (parseContent . dropWhile isSpace) $ endByOneOf ",." cts
    parseContent (n:' ':color) = (digitToInt n, unwords $ init $ words color)
    parseContent _ = (0, "")

getRules :: IO [Rule]
getRules = map parse . lines <$> readFile "input.txt"

findInRules :: [Rule] -> BagColor -> Rule
findInRules rules color = fromJust $ find ((== color) . bagColor) rules

part1 :: IO Int
part1 = do
  rules <- getRules
  let contains (Rule _ cts) = liftA2 (||) ("shiny gold" `elem`) (any (contains . findInRules rules)) (map snd cts)
  pure $ length $ filter contains rules

part2 :: IO Int
part2 = flip bagsIn "shiny gold" <$> getRules
  where
    bagsIn rules = sum . map (\(n, c) -> n * (1 + bagsIn rules c)) . contents . findInRules rules
