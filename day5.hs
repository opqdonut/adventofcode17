import qualified Data.IntMap.Strict as M
import Data.Maybe

type State = M.IntMap Int

upd :: Int -> Int
upd x
  | x >= 3 = x - 1
  | otherwise = x + 1

jump :: (State,Int) -> (State,Int)
jump (state,pos) = (M.adjust upd pos state, pos + rel)
  where rel = fromJust $ M.lookup pos state

escapeTime :: State -> Int
escapeTime state = go state 0 0
  where go state steps pos
          | M.member pos state = let (state',pos') = jump (state,pos)
                                 in go state' (steps+1) pos'
          | otherwise = steps

getInput :: IO State
getInput = fmap (M.fromList . zip [0..] . map read . lines) $ readFile "input.5.txt"

testInput :: State
testInput = M.fromList [(0,0),(1,3),(2,0),(3,1),(4,-3)]

main = getInput >>= print . escapeTime
