import qualified Data.Map.Strict as M
import Data.List

type Register = String
data Command = Add Register Int
data Condition = CLT Register Int
               | CLE Register Int
               | CEQ Register Int
               | CGE Register Int
               | CGT Register Int
               | CNE Register Int
data Instruction = Instruction Command Condition

parseLine :: String -> Instruction
parseLine s = p (words s)
  where p [target, command, amount, "if", reference, op, value] =
          Instruction (mkCommand command target amount) (mkCondition op reference value)
        mkCommand "inc" target amount = Add target (read amount)
        mkCommand "dec" target amount = Add target (negate (read amount))
        mkCondition "<" r v  = CLT r (read v)
        mkCondition "<=" r v = CLE r (read v)
        mkCondition "==" r v = CEQ r (read v)
        mkCondition ">=" r v = CGE r (read v)
        mkCondition ">" r v  = CGT r (read v)
        mkCondition "!=" r v = CNE r (read v)

type State = M.Map Register Int

get :: Register -> State -> Int
get k m = M.findWithDefault 0 k m

put :: Register -> Int -> State -> State
put = M.insert

initial :: State
initial = M.empty

evalCondition :: State -> Condition -> Bool
evalCondition s (CLT r v) = get r s < v
evalCondition s (CLE r v) = get r s <= v
evalCondition s (CEQ r v) = get r s == v
evalCondition s (CGE r v) = get r s >= v
evalCondition s (CGT r v) = get r s > v
evalCondition s (CNE r v) = get r s /= v

evalCommand :: State -> Command -> State
evalCommand s (Add r v) = put r (get r s + v) s

eval :: State -> Instruction -> State
eval s (Instruction comm cond)
  | evalCondition s cond = evalCommand s comm
  | otherwise = s

run :: State -> [Instruction] -> [State]
run = scanl eval

largest states = maximum (concatMap M.elems states)

input = fmap (map parseLine . lines) (readFile "input.8.txt")

main = input >>= print . largest . run initial
