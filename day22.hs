import qualified Data.Map.Strict as M

type XY = (Int,Int)

data Node = Clean | Weakened | Infected | Flagged
  deriving Show

type World = M.Map XY Node

parsePoint '.' = Clean
parsePoint '#' = Infected

parseMap :: String -> World
parseMap s = M.fromList coords
  where ls = lines s
        w = length (head ls)
        h = length ls
        startY = -(h-1)`div`2
        startX = -(w-1)`div`2
        coords = [((x,y),parsePoint c) | (y, row) <- zip [startY..] ls
                                       , (x, c) <- zip [startX..] row]

getMap = fmap parseMap $ readFile "input.22.txt"

data Dir = U | L | D | R
  deriving Show

turnLeft U = L
turnLeft L = D
turnLeft D = R
turnLeft R = U

turnRight U = R
turnRight L = U
turnRight D = L
turnRight R = D

move :: XY -> Dir -> XY
move (x,y) D = (x,y+1)
move (x,y) U = (x,y-1)
move (x,y) L = (x-1,y)
move (x,y) R = (x+1,y)

example = parseMap "..#\n#..\n..."

data State = State { world :: !World, pos :: !XY, dir :: !Dir, count :: !Int }
  deriving Show

getNode :: World -> XY -> Node
getNode w xy = M.findWithDefault Clean xy w

putNode :: World -> XY -> Node -> World
putNode w xy Clean = M.delete xy w
putNode w xy n = M.insert xy n w

processNode :: Node -> Node
processNode Clean = Weakened
processNode Weakened = Infected
processNode Infected = Flagged
processNode Flagged = Clean

initialState :: World -> State
initialState w = State w (0,0) U 0

step :: State -> State
step (State world pos dir count) = State newWorld (move pos newDir) newDir newCount
  where node = getNode world pos
        newNode = processNode node
        newWorld = putNode world pos newNode
        newDir = case node of Clean -> turnLeft dir
                              Weakened -> dir
                              Infected -> turnRight dir
                              Flagged -> turnRight $ turnRight dir
        newCount = case newNode of Infected -> count + 1
                                   _ -> count

render :: State -> IO ()
render (State w pos dir count) = putStrLn $ unlines $ show count : map row [minY..maxY]
  where (xs,ys) = unzip $ pos : M.keys w
        minX = minimum xs
        maxX = maximum xs
        minY = minimum ys
        maxY = maximum ys
        row r = concatMap (place r) [minX..maxX]
        place y x
          | pos == (x,y) = "[" ++ point (getNode w (x,y)) ++ show dir
          | otherwise    = " " ++ point (getNode w (x,y)) ++ " "
        point Clean = "."
        point Infected = "#"
        point Weakened = "W"
        point Flagged = "F"

nTimes :: Int -> (a->a) -> a -> a
nTimes 0 _ x = x
nTimes n f x = nTimes (n-1) f (f x)

part2 m = count $ nTimes 10000000 step (initialState m)

main = getMap >>= print . part2
