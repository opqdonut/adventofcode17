import qualified Data.Map.Lazy as M
import Data.Maybe
import Data.List (sortBy,all)
import Data.Ord

data Node = Node Int [String]
  deriving Show

parseLine :: String -> (String,Node)
parseLine s = process (words s)
  where process (s:('(':num):"->":rest) = (s, Node (read (init num)) (map (filter (/=',')) rest))
        process (s:('(':num):[]) = (s, Node (read (init num)) [])

type Ann a = M.Map String a
type Tree = Ann Node

parseFile :: String -> Tree
parseFile = M.fromList . map parseLine . lines

getInput :: IO Tree
getInput = fmap parseFile $ readFile "input.7.txt"

invert :: Tree -> Ann String
invert tree = M.fromList [ (child, parent)
                         | (parent, Node _ children) <- M.assocs tree
                         , child <- children ]

findRoot :: Tree -> String
findRoot tree = go start
  where (start, _):_ = M.assocs tree
        parents = invert tree
        go x = case M.lookup x parents of Nothing -> x
                                          Just y -> go y

find :: Ann a -> String -> a
find m k = fromJust $ M.lookup k m

weights :: Tree -> Ann Int
weights tree = ws
  where ws = M.map weight tree
        weight (Node w children) = w + sum (map (find ws) children)

sumMaybes :: [Maybe a] -> Maybe a
sumMaybes = listToMaybe . catMaybes

allEqual :: Eq a => [a] -> Bool
allEqual [] = True
allEqual (x:xs) = all (==x) xs

findUnbalanced :: Tree -> Ann Int -> String -> Maybe String
findUnbalanced tree weights name
  | isJust recurse = recurse
  | allEqual (map (find weights) children) = Nothing
  | otherwise = Just name
  where Node _ children = find tree name
        recurse = sumMaybes $ map (findUnbalanced tree weights) children

balance :: Tree -> Ann Int -> String -> Int
balance tree weights name = referenceWeight - oddTotalWeight + oddWeight
  where (Node _ children) = find tree name
        -- assume: at least three children
        -- odd one out is either the first or last when sorted
        sorted = sortBy (comparing (find weights)) children
        light:reference:_ = sorted
        heavy = last sorted
        referenceWeight = find weights reference
        oddOneOut = if find weights light /= referenceWeight then light else heavy
        oddTotalWeight = find weights oddOneOut
        Node oddWeight _ = find tree oddOneOut

compute tree = balance tree ws . fromJust $ findUnbalanced tree ws root
  where ws = weights tree
        root = findRoot tree

answer = fmap compute getInput
