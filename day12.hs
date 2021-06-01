import qualified Data.IntMap.Strict as M
import Control.Monad.State

parseLine :: String -> (Int, [Int])
parseLine = p . words
  where p (a:"<->":rest) = (read a, map read' rest)
        read' = read . filter (/=',')

parse :: String -> [(Int, [Int])]
parse = map parseLine . lines

type Graph = M.IntMap [Int]

mkGraph :: [(Int, [Int])] -> Graph
mkGraph = M.fromList

type Nodes = M.IntMap ()

visit :: Graph -> Int -> State Nodes ()
visit g i = do
  modify (M.insert i ())
  visited <- get
  let neighbors = g M.! i
  mapM_ (visit g) $ filter (\x -> M.notMember x visited) neighbors

dfs :: Graph -> Int -> Nodes
dfs g start = execState (visit g start) M.empty

components :: Graph -> Int
components g = case M.keys g of
  [] -> 0
  (x:_) -> 1 + components (M.difference g (dfs g x))

main = readFile "input.12.txt" >>= print . components . mkGraph . parse
