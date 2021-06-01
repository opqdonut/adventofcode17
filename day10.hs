import Data.Char
import Data.Bits (xor)
import Numeric

data CyclicZipper a = CyclicZipper Int Int [a]
  deriving Show

zipper :: [a] -> CyclicZipper a
zipper xs = CyclicZipper (length xs) 0 xs

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

unzipper :: CyclicZipper a -> [a]
unzipper (CyclicZipper len pos xs) = rotate (mod (len-pos) len) xs

move :: Int -> CyclicZipper a -> CyclicZipper a
move n (CyclicZipper len pos xs) = CyclicZipper len (mod (pos+n') len) (rotate n' xs)
  where n' = mod n len

twist :: Int -> CyclicZipper a -> CyclicZipper a
twist n (CyclicZipper l p xs) = CyclicZipper l p (reverse (take n xs) ++ drop n xs)

-- -- --

compute :: Int -> [Int] -> CyclicZipper a -> CyclicZipper a
compute _ [] z = z
compute skip (len:lens) z = compute (succ skip) lens z'
  where z' = move skip . move len . twist len $ z

performTwists :: [Int] -> [Int] -> [Int]
performTwists input lens = unzipper . compute 0 lens $ zipper input

output :: [Int] -> String
output [] = []
output xs = text ++ output rest
  where (now,rest) = splitAt 16 xs
        out = foldl1 xor now
        text = pad $ showHex out ""
        pad [a,b] = [a,b]
        pad [a]   = ['0',a]

multiply 0 _ = []
multiply n xs = xs ++ multiply (pred n) xs

salt = [17, 31, 73, 47, 23]

hash :: String -> String
hash dat = output $ performTwists [0..255] (multiply 64 (lens++salt))
  where lens = map ord dat

-- -- --

main = readFile "input.10.txt" >>= print . hash . init
