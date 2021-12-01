module Day03 where

solve :: (Int, Int) -> IO Int
solve (right, down) = fst . foldl f (0, (0, 0)) . map cycle . lines <$> readFile "input.txt"
  where
    f (treeCount, (x, 0)) line =
      let newCoords = (x + right, down - 1)
          trees = if line !! x == '#' then 1 else 0
      in (treeCount + trees, newCoords)
    f (treeCount, (x, y)) _ = (treeCount, (x, y - 1))

{- Part 1:
Starting at the top-left corner of your map and following a slope of right 3 and
down 1, how many trees would you encounter?
-}
part1 :: IO Int
part1 = solve (3, 1)

{- Part 2:
Determine the number of trees you would encounter if, for each of the following
slopes, you start at the top-left corner and traverse the map all the way to the
bottom:

- Right 1, down 1
- Right 3, down 1
- Right 5, down 1
- Right 7, down 1
- Right 1, down 2

What do you get if you multiply together the number of trees encountered on each
of the listed slopes?
-}
part2 :: IO Int
part2 = product <$> mapM solve [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
