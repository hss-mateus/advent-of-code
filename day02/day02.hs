module Day02 where

import Data.List

data Entry = Entry Int Int Char String

parse :: String -> Entry
parse entry = Entry a b char' password
  where
    [ab, char, password] = words entry
    (a, b) = let (a', '-':b') = span (/= '-') ab
             in (read a', read b')
    char':_ = char

solve :: (Entry -> Bool) -> IO Int
solve validator = length . filter validator . map parse . lines <$> readFile "input.txt"

{- Part 1:
The password policy indicates the lowest and highest number of times a given
letter must appear for the password to be valid. For example, "1-3 a" means that
the password must contain a at least 1 time and at most 3 times.

How many passwords are valid according to their policies?
-}
validate1 :: Entry -> Bool
validate1 (Entry lo hi char password) = occurrences `elem` [lo..hi]
  where occurrences = length (filter (== char) password)

part1 :: IO Int
part1 = solve validate1

{- Part 2:
Each policy actually describes two positions in the password, where 1 means the
first character, 2 means the second character, and so on. Exactly one of these
positions must contain the given letter. Other occurrences of the letter are
irrelevant for the purposes of policy enforcement.

How many passwords are valid according to the new interpretation of the policies?
-}
validate2 :: Entry -> Bool
validate2 (Entry first second char password) = length occurrences == 1
  where occurrences = [pred first, pred second] \\ elemIndices char password

part2 :: IO Int
part2 = solve validate2
