import qualified Data.Map.Strict as M
import Data.List

lastOnShell n = (1+2*n)^2

input = 347991 :: Int

shellOf n = succ . last . takeWhile (\i -> lastOnShell i <= n) $ [0..]

indexOn shell n = n - (lastOnShell (pred shell) + 1)

shapeOf shell = right ++ top ++ left ++ bottom
  where start = (shell, shell-1)
        right =  [(shell, y) | y <- [shell-1,shell-2..negate shell]]
        top =    [(x,-shell) | x <- [shell-1,shell-2..negate shell]]
        left =   [(-shell,y) | y <- [-shell+1..shell]]
        bottom = [(x,shell)  | x <- [-shell+1..shell]]

position' shell n = shapeOf shell !! indexOn shell n

position n = position' (shellOf n) n

distance (x,y) = abs x + abs y

main1 = print . distance . position $ input

--- part 2

type Grid = M.Map (Int,Int) Int

sumNeighbors :: Grid -> (Int,Int) -> Int
sumNeighbors m (x,y) = sum . map (\i -> M.findWithDefault 0 i m) $
  [(x-1,y-1),
   (x,y-1),
   (x+1,y-1),
   (x+1,y),
   (x+1,y+1),
   (x,y+1),
   (x-1,y+1),
   (x-1,y)]

evalShell :: Int -> Grid -> Grid
evalShell shell m = foldl' (\m xy -> M.insert xy (sumNeighbors m xy) m) m (shapeOf shell)

start :: Grid
start = M.insert (0,0) 1 M.empty

findFirstLargerThan thresh = go start 0 []
  where go m shell [] = go m (shell+1) (shapeOf (shell+1))
        go m shell (c:cs)
          | val > thresh = (val,c)
          | otherwise = go (M.insert c val m) shell cs
          where val = sumNeighbors m c

main2 = print $ findFirstLargerThan input
