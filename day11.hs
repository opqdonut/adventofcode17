import Data.Monoid
import Data.List
import Data.Ord

data Hex = Hex Int Int Int
  deriving Show

instance Monoid Hex where
  mempty = Hex 0 0 0
  mappend (Hex a b c) (Hex x y z) = Hex (a+x) (b+y) (c+z)

invert :: Hex -> Hex
invert (Hex a b c) = Hex (negate a) (negate b) (negate c)

--   ____/  \___\____
--  /\   \ 101  /\   \              z
-- /  \___\____/  \___\          x  |
-- \ 1-10 /\   \ 011  /           \ |
--  \____/  \___\____/             \|___y
--  /\   \ 000  /\   \
-- /  \___\____/  \___\
-- \0-1-1 /\   \ -110 /
--  \____/  \___\____/
--       \-10-1 /


north = Hex 1 0 1
ne    = Hex 0 1 1
se    = Hex (-1) 1 0
south = Hex (-1) 0 (-1)
sw    = Hex 0 (-1) (-1)
nw    = Hex 1 (-1) 0

norm :: Hex -> Int
norm (Hex a b c) = maximum $ map abs [a,b,c]

parse :: String -> [Hex]
parse s = case break (==',') s of
  (w,[]) -> [p w]
  (w,',':rest) -> p w : parse rest
  where p "n" = north
        p "nw" = nw
        p "ne" = ne
        p "s" = south
        p "sw" = sw
        p "se" = se

main = readFile "input.11.txt" >>= print . maximum . map norm . scanl1 mappend . parse . init
