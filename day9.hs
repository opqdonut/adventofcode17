data State = State {level :: Int, score :: Int, garbages :: Int, string :: String}
  deriving Show
data Parser = Parser {runParser :: State -> State}

(|>) :: Parser -> Parser -> Parser
Parser p |> Parser q = Parser (q.p)

peek :: State -> (Char,State)
peek (State l s g (x:xs)) = (x, State l s g xs)

choice :: (Char -> Parser) -> Parser
choice f = Parser p
  where p state = let (c,state') = peek state in runParser (f c) state'

nothing :: Parser
nothing = Parser id

-- -- --

scoreGroup :: Parser
scoreGroup = Parser (\(State level score garbages string) -> (State level (score+level) garbages string))

countGarbage :: Parser
countGarbage = Parser (\(State level score garbages string) -> (State level score (succ garbages) string))

nextLevel :: Parser -> Parser
nextLevel p = modifyLevel succ |> p |> modifyLevel pred
  where modifyLevel f = Parser (\(State level score garbages string) -> State (f level) score garbages string)

consume :: Parser
consume = Parser f
  where f (State level score garbages (c:cs)) = (State level score garbages cs)

-- -- --

garbage :: Parser
garbage = choice g
  where g '!' = consume |> garbage
        g '>' = nothing
        g c   = countGarbage |> garbage

beginGroup :: Parser
beginGroup = nextLevel group

group :: Parser
group = choice g
  where g '}' = scoreGroup
        g '{' = beginGroup |> moreThings
        g '<' = garbage |> moreThings
        moreThings = choice more
        more '}' = scoreGroup
        more ',' = group

thing :: Parser
thing = choice f
  where f '{' = beginGroup
        f '<' = garbage

-- -- --

parse s = runParser thing (State 0 0 0 s)

main = readFile "input.9.txt" >>= print . parse
