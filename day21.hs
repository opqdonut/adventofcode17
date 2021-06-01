import qualified Data.Map.Strict as M

import Data.Maybe
import Data.List

type Image = [String]

flipH :: [[a]] -> [[a]]
flipH = map reverse

flipT :: [[a]] -> [[a]]
flipT = transpose

orientations :: Image -> [Image]
orientations i = map o [0..7]
  where o 0 = i
        o n
          | even n = flipH (o (n-1))
          | otherwise = flipT (o (n-1))

showI = unlines

initial = [".#.", "..#", "###"]

example = ["#..#","#...","..#.","#..#"]

slice :: Int -> [a] -> [[a]]
slice k = takeWhile (not.null) . map (take k) . iterate (drop k)

split :: Int -> Image -> [[Image]]
split k i = map flipT $ slice k $ map (slice k) i

unsplit :: [[Image]] -> Image
unsplit is = concatMap row is
  where row = map concat . transpose

type Book = M.Map Image Image

match :: Book -> Image -> Image
match b i = go (orientations i)
  where go (j:js) = case M.lookup j b of Just k -> k
                                         Nothing -> go js
        go [] = error $ "no match for " ++ show i

step b i = unsplit $ (map.map) (match b) splitted
  where splitted
          | even (length i) = split 2 i
          | otherwise = split 3 i

exampleBook = M.fromList [
  (["..",".#"], ["##.","#..","..."]),
  ([".#.", "..#", "###"], ["#..#","....","....","#..#"])
  ]

parseImage :: String -> Image
parseImage = lines . map repl
  where repl '/' = '\n'
        repl x = x

parseLine :: String -> (Image, Image)
parseLine = p . words
  where p [i, "=>", j] = (parseImage i, parseImage j)

parseBook :: String -> Book
parseBook = M.fromList . map parseLine . lines

getBook :: IO Book
getBook = fmap parseBook $ readFile "input.21.txt"

count :: Image -> Int
count = length . filter (=='#') . concat

part1 b = count $ iterate (step b) initial !! 5

part2 b = count $ iterate (step b) initial !! 18

main = getBook >>= print . part2
