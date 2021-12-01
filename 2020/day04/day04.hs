{-# LANGUAGE LambdaCase #-}

module Day04 where

import Data.Char
import Data.List
import Data.List.Split
import Control.Applicative
import qualified Data.Map as M

validateKeys :: [String] -> Bool
validateKeys = (== validKeys) . sort . filter (/= "cid")
  where validKeys = ["byr", "ecl", "eyr", "hcl", "hgt", "iyr", "pid"]

part1 :: IO Int
part1 = length . filter validate . splitOn "\n\n" <$> readFile "input.txt"
  where validate = validateKeys . map (takeWhile (/= ':')) . words

part2 :: IO Int
part2 = length . filter (validate . parseEntry) . splitOn "\n\n" <$> readFile "input.txt"
  where
    parseEntry = M.fromList . map ((\[k, v] -> (k, v)) . splitOn ":") . words
    validate = liftA2 (&&) and (validateKeys . M.keys) . M.mapWithKey f
    f = \case
      "byr" -> validateDate [1920..2002]
      "iyr" -> validateDate [2010..2020]
      "eyr" -> validateDate [2020..2030]
      "hgt" -> validateHeight
      "ecl" -> validateEyeColor
      "hcl" -> validateHairColor
      "pid" -> validatePid
      _     -> const True

    validateHeight str =
      case span isDigit str of
        (n, "cm") | read n `elem` [150..193] -> True
        (n, "in") | read n `elem` [59..76]   -> True
        _                                    -> False

    validateDate = flip (elem . read)

    validateEyeColor = (`elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"])

    validateHairColor ('#':xs) = all (`elem` ['0'..'9'] ++ ['a'..'f']) xs
    validateHairColor _ = False

    validatePid str = length str == 9 && all isDigit str
