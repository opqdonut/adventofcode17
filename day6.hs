import Data.List
import Data.Ord
import Data.Map as M

input :: [Int]
input = [10,3,15,10,5,15,5,15,9,2,5,8,5,2,3,6]

zipWithRemainder :: (a->a->a) -> [a] -> [a] -> ([a],[a])
zipWithRemainder f (x:xs) (y:ys) = let (zs,rest) = zipWithRemainder f xs ys in (f x y : zs, rest)
zipWithRemainder f []     ys     = ([],ys)
zipWithRemainder f xs     []     = (xs,[])

(+++) :: [Int] -> [Int] -> [Int]
xs +++ [] = xs
xs +++ ys = res +++ rest
  where (res,rest) = zipWithRemainder (+) xs ys

spread :: [Int] -> Int -> [Int]
spread xs pos = xs' +++ deltas
  where (left,amount:right) = splitAt pos xs
        xs' = left ++ [0] ++ right
        deltas = replicate (pos+1) 0 ++ replicate amount 1

step :: [Int] -> [Int]
step xs = spread xs pos
  -- oh bother, maximumBy returns the last max
  where (pos,_) = maximumBy (comparing snd) . reverse $ zip [0..] xs

cycleLength xs = go M.empty 0 xs
  where go seen i xs = case M.lookup xs seen of
          Just j -> i-j
          Nothing -> go (M.insert xs i seen) (i+1) (step xs)

answer = cycleLength input
