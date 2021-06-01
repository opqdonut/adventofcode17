import Data.Array
import Data.Char
import Data.List

type Maze = Array (Int,Int) Char

parse :: String -> Maze
parse s = array ((0,0),(w,h)) coords
  where ls = lines s
        w = length (head ls) - 1
        h = length ls - 1
        coords = [ ((x,y),c) | (y,line) <- zip [0..] ls, (x,c) <- zip [0..] line ]

data Dir = U | D | L | R
  deriving (Show,Eq)

opposite U = D
opposite D = U
opposite L = R
opposite R = L

step (x,y) U = (x,y-1)
step (x,y) D = (x,y+1)
step (x,y) L = (x-1,y)
step (x,y) R = (x+1,y)

neighbours c = map (step c) [U,D,L,R]
vertical (x,y) = [(x,y-1),(x,y+1)]

path ' ' = False
path _   = True

cross '+' = True
cross _   = False

nextCross arr dir c = filter possible $ delete (opposite dir) [U,D,L,R]
  where possible dir' = path (arr ! step c dir')

next arr dir c
  | cross (arr ! c) = let [out] = nextCross arr dir c in Just out
  | path (arr ! step c dir) = Just dir
  | otherwise = Nothing

walk :: Maze -> Dir -> (Int,Int) -> [((Int,Int), Char)]
walk arr dir c = (c, arr ! c) : case next arr dir c of
                                  Just dir' -> walk arr dir' (step c dir')
                                  Nothing -> []

getInput = fmap parse $ readFile "input.19.txt"

part1 = do i <- getInput
           print . filter isAlpha . map snd $ walk i D (103,0)

part2 = do i <- getInput
           print . length $ walk i D (103,0)
