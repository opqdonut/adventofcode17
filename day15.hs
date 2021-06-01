import Data.Bits ((.&.))


initA, initB, multA, multB, p, limit :: Int
initA = 516
multA = 16807
initB = 190
multB = 48271
p = 2147483647
limit = 40*1000*1000
testMask = 0xffff

generate :: Int -> Int -> Int -> [Int]
generate init mult p = tail $ iterate (\x -> mod (x * mult) p) init

genA = generate initA multA p
genB = generate initB multB p

equal :: Int -> Int -> Bool
equal x y = (x .&. testMask) == (y .&. testMask)

answer1 = length . filter id . take limit $ zipWith equal genA genB

--

limit2, divA, divB :: Int
limit2 = 5*1000*1000
divA = 4
divB = 8

keep d = filter (\x -> mod x d == 0)

genA2 = keep divA genA
genB2 = keep divB genB

answer2 = length . filter id . take limit2 $ zipWith equal genA2 genB2

answer2test = length . filter id . take limit2 $ zipWith equal a b
  where a = keep divA $ generate 65 multA p
        b = keep divB $ generate 8921 multB p

main = print answer2
