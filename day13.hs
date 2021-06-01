data Wall = Wall {depth :: Int, range :: Int}
  deriving Show

parseLine :: String -> Wall
parseLine = p . words
  where p [d,r] = Wall (read $ init d) (read r)

parse :: String -> [Wall]
parse = map parseLine . lines

getInput :: IO [Wall]
getInput = fmap parse $ readFile "input.13.txt"

catch :: Wall -> Int -> Bool
catch (Wall d r) delay = mod (d+delay) period == 0
  where period = 2 * (r - 1)

severity :: Wall -> Int
severity w
  | catch w 0 = depth w * range w
  | otherwise = 0

answer1 ws = sum $ map severity ws

-- part 2

intersectSorted (x:xs) (y:ys)
  | x == y = x:intersectSorted xs ys
  | x < y  = intersectSorted xs (y:ys)
  | otherwise = intersectSorted (x:xs) ys

catchTimes :: Wall -> [Int]
catchTimes w = filter (not . catch w) [0..]

catchTimesAll :: [Wall] -> [Int]
catchTimesAll = foldr1 intersectSorted . map catchTimes

answer2 = head . catchTimesAll

main = getInput >>= print . answer2
