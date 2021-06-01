import qualified Data.Map.Strict as M
import Text.Read (readMaybe)

type R = Char

data Arg = Literal Int | Register R
  deriving Show

data Command = Snd Arg
             | Set R Arg
             | Add R Arg
             | Mul R Arg
             | Mod R Arg
             | Rcv R
             | Jgz Arg Arg
  deriving Show

parseRegister :: String -> R
parseRegister [c] = c

parseArg :: String -> Arg
parseArg w = case readMaybe w of
               Just i -> Literal i
               Nothing -> Register $ parseRegister w

parseCommand :: String -> Command
parseCommand = p . words
  where p ["snd",x] = Snd (parseArg x)
        p ["set",x,y] = Set (parseRegister x) (parseArg y)
        p ["add",x,y] = Add (parseRegister x) (parseArg y)
        p ["mul",x,y] = Mul (parseRegister x) (parseArg y)
        p ["mod",x,y] = Mod (parseRegister x) (parseArg y)
        p ["rcv",x] = Rcv (parseRegister x)
        p ["jgz",x,y] = Jgz (parseArg x) (parseArg y)

parse :: String -> [Command]
parse = map parseCommand . lines

getInput = fmap parse $ readFile "input.18.txt"

data Status = Run | Blocked
  deriving (Show, Eq)
data State = State {id :: !Int, status :: !Status, pc :: !Int, mem :: !(M.Map Char Int), queue :: ![Int], sends :: !Int, send :: ![Int]}
  deriving Show

getRegister :: State -> R -> Int
getRegister s r = M.findWithDefault 0 r (mem s)

putRegister :: State -> R -> Int -> State
putRegister s r v = s { mem = M.insert r v $ mem s }

evalArg :: State -> Arg -> Int
evalArg _ (Literal i) = i
evalArg s (Register r) = getRegister s r

step s = s { pc = succ (pc s) }

evalOp f s r x = step $ putRegister s r (f (getRegister s r) (evalArg s x))

evalCommand :: State -> Command -> State
evalCommand s (Snd x) = step s { send = send s ++ [evalArg s x], sends = sends s + 1 }
evalCommand s (Set r x) = evalOp (\_ val -> val) s r x
evalCommand s (Add r x) = evalOp (+) s r x
evalCommand s (Mul r x) = evalOp (*) s r x
evalCommand s (Mod r x) = evalOp mod s r x
evalCommand s (Rcv r) = case queue s of [] -> s {status = Blocked}
                                        (x:xs) -> step $ putRegister s{queue=xs, status=Run} r x
evalCommand s (Jgz r x)
  | evalArg s r > 0 = s { pc = pc s + evalArg s x }
  | otherwise = step s

evalProg :: State -> [Command] -> State
evalProg s cs = evalCommand s $ cs !! pc s

initialState id = putRegister (State id Run 0 M.empty [] 0 []) 'p' id

evalDuet :: [Command] -> (State, State) -> (State, State)
evalDuet prog (s1, s2) = let s1' = evalProg s1 prog
                             s2' = evalProg s2 prog
                             send1 = send s1'
                             send2 = send s2'
                             s1'' = s1' { send=[], queue=queue s1' ++ send2}
                             s2'' = s2' { send=[], queue=queue s2' ++ send1}
                         in (s1'',s2'')

runDuet prog = go (initialState 0, initialState 1)
  where go (s1, s2)
          | status s1 == Blocked && status s2 == Blocked = (s1, s2)
          | otherwise = go $ evalDuet prog (s1, s2)

example = [Snd (Literal 1), Snd (Literal 2), Snd (Register 'p'), Rcv 'a', Rcv 'b', Rcv 'c', Rcv 'd']

main = getInput >>= print . runDuet
