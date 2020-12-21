{-# LANGUAGE LambdaCase #-}

module Day08 where

import qualified Data.Map as M

data Instruction = Acc Int
                 | Jmp Int
                 | Nop Int
                 deriving Eq

type Lines = M.Map Int Instruction

data State = State
  { accumulator :: Int
  , currentLine :: Int
  , history     :: [Int]
  }

data EvalResult = InfiniteLoop Int
                | End Int
                deriving Show

parse :: String -> Lines
parse = M.fromList . zip [0..] . map parseLine . lines . filter (/= '+')
  where
    parseLine line =
      case splitAt 4 line of
        ("nop ", n) -> Nop (read n)
        ("acc ", n) -> Acc (read n)
        ("jmp ", n) -> Jmp (read n)

eval :: Lines -> EvalResult
eval lines = go (State 0 0 [])
  where
    go (State acc lineNum hist)
      | lineNum `elem` hist     = InfiniteLoop acc
      | nextLine > M.size lines = End acc
      | otherwise               = go newState
      where
        nextLine = lineNum + 1
        newHist = lineNum:hist
        newState =
          case (M.!) lines lineNum of
            Nop _ -> State acc nextLine newHist
            Acc n -> State (acc + n) nextLine newHist
            Jmp n -> State acc (lineNum + n) newHist

part1 :: IO EvalResult
part1 = eval . parse <$> readFile "input.txt"

part2 :: IO EvalResult
part2 = fix 0 . parse <$> readFile "input.txt"
  where
    fix lineNum lines =
      case eval lines of
        End acc        -> End acc
        InfiniteLoop _ -> fix nextLine . M.adjust toggle nextLine $ M.adjust toggle lineNum lines
      where
        nextLine = lineNum + 1
        toggle = \case
          Nop n -> Jmp n
          Jmp n -> Nop n
          acc   -> acc
