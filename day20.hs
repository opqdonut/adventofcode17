import Data.Ord
import Data.List
import Data.Function
import Data.Maybe
import qualified Data.IntMap.Strict as M

data Vec3 = Vec3 Integer Integer Integer
  deriving (Show, Eq, Ord)

scale a (Vec3 x y z) = Vec3 (a*x) (a*y) (a*z)
plus (Vec3 x y z) (Vec3 a b c) = Vec3 (x+a) (y+b) (z+c)
norm (Vec3 x y z) = abs x + abs y + abs z

parseVec :: String -> Vec3
parseVec ('<':rest) = Vec3 (read x) (read y) (read z)
  where (x,',':yz) = break (==',') rest
        (y,',':z0) = break (==',') yz
        z = init z0

data Particle = Particle {name :: Int, pos :: Vec3, vel :: Vec3, acc :: Vec3}
  deriving Show

parseParticle :: Int -> String -> Particle
parseParticle name = p . words
  where p ['p':'=':pos0, 'v':'=':vel0, 'a':'=':acc] =
          Particle name (parseVec $ init pos0) (parseVec $ init vel0) (parseVec acc)

posAt :: Integer -> Particle -> Vec3
posAt t (Particle _ p v a) = p `plus` scale t v `plus` scale (div (t*(t+1)) 2) a

closer :: Particle -> Particle -> Ordering
closer = comparing (norm.acc)

getInput = fmap (zipWith parseParticle [0..] . lines) $ readFile "input.20.txt"

part1 ps = minimumBy closer ps

-- part2

collide :: Integer -> [Particle] -> [Particle]
collide t parts = map (snd.head) . filter unique . groupBy ((==)`on`fst) . sortOn fst $ possed
  where possed = map (\p -> (posAt t p, p)) parts
        unique [x] = True
        unique _   = False

example = [parseParticle 0 "p=<-6,0,0>, v=<3,0,0>, a=<0,0,0>",
           parseParticle 1 "p=<-4,0,0>, v=<2,0,0>, a=<0,0,0>",
           parseParticle 2 "p=<-2,0,0>, v=<1,0,0>, a=<0,0,0>",
           parseParticle 3 "p=<3,0,0>, v=<-1,0,0>, a=<0,0,0>"]

isqrt :: Integer -> [Integer]
isqrt x = if s ^ 2 == x then [s] else []
  where s = round $ sqrt $ fromIntegral x

idiv :: Integer -> Integer -> [Integer]
idiv x y = case divMod x y of (d,0) -> [d]
                              _ -> []

data Solutions = All | Some [Integer]
  deriving Show

smallestPositive :: Solutions -> Maybe Integer
smallestPositive All = Just 0
smallestPositive (Some xs) = listToMaybe . sort $ filter (>=0) xs

combine :: Solutions -> Solutions -> Solutions
combine All x   = x
combine x   All = x
combine (Some xs) (Some ys) = Some $ intersect xs ys

combineAll :: [Solutions] -> Solutions
combineAll xs = foldr combine All xs

integerSolution :: Integer -> Integer -> Integer -> Solutions
integerSolution 0 0 0 = All
integerSolution 0 0 c = Some []
-- -c/b
integerSolution 0 b c = Some $ idiv (-c) b
-- (-b +- sqrt(b^2-4ac))/2a
integerSolution a b c = Some $
  do s <- isqrt (b^2 - 4*a*c)
     candidate <- [-b+s, -b-s]
     idiv candidate (2*a)

collisions :: Particle -> Particle -> Solutions
collisions (Particle _ p1 v1 a1) (Particle _ p2 v2 a2) =
  -- solve p1 + t*v1 + t*(t-1)*a1/2 == p2 + t*v2 + t*(t+1)*a2/2
  --   <=> (p1-p2) + t*(v1-v2) + t*(t+1)*(a1-a2)/2 == 0
  --   <=> 2*(p1-p2) + t*(2*(v1-v2)+(a1-a2)) + t*t*(a1-a2)
  let (Vec3 px py pz) = plus p1 $ scale (-1) p2
      (Vec3 vx vy vz) = plus v1 $ scale (-1) v2
      (Vec3 ax ay az) = plus a1 $ scale (-1) a2
      solve p v a = integerSolution a (2*v+a) (2*p)
  in combineAll [solve px vx ax,
                 solve py vy ay,
                 solve pz vz az]

canCollide a b = isJust $ smallestPositive $ collisions a b

canCollide2 n a b = any (\t -> posAt t a == posAt t b) [0..n]

example2 = [parseParticle 0 "p=<1209,168,-645>, v=<-85,39,108>, a=<-7,-9,-9>",
            parseParticle 1 "p=<339,-537,-720>, v=<-97,5,66>, a=<11,10,0>",
            parseParticle 2 "p=<374,-947,120>, v=<26,68,4>, a=<-12,6,-4>"]

allCollisions :: [Particle] -> [(Int,[Int])]
allCollisions parts = [(fromIntegral t,[name a,name b]) | (a:choices) <- tails parts, b <- choices, t <- maybeToList $ smallestPositive $ collisions a b]

collisionMap :: [Particle] -> [[Int]]
collisionMap = M.elems . M.fromListWith (\as bs -> nub $ as ++ bs) . allCollisions

survivors ps = go (map name ps) (collisionMap ps)
  where go alive [] = alive
        go alive (dead:rest) = go (alive\\dead) (map (\\dead) rest)

part2 = length . survivors

main = getInput >>= print . part2
