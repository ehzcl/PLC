import Data.List
import Data.Char

-- questao 01

mmax :: (a -> Int) -> a -> a -> a
mmax f a b = if l > r then a else b
  where l = f a
        r = f b


-- questao 02

countSorted :: [String] -> Int
countSorted n = (length . filter (\x -> x == sort x) ) n


-- questao 03

mStr :: [String] -> String
mStr n = (map toUpper . intercalate [' '] . filter (\x-> (length x) > 5)) n


-- questao 04 (melhorar)
{-
powersOf' :: Integer -> [Integer] -> Integer -> [Integer]
powersOf' n k m
 | n^(head k) > m = []
 | otherwise = (n^(head k)):(powersOf' n (tail k) m)
-}

powersOf'' :: Integer -> Integer -> [Integer] -> [Integer]
powersOf'' n k l= if n * (last l) > k then l else powersOf'' n k (l ++ [n*(last l)])


powersOf :: Integer -> Integer -> [Integer]
powersOf n k = powersOf'' n k [1, n]

-- questao 05

sumUntil :: Int -> [Int] -> [Int]
sumUntil index list = ((sum . (take index)) list):[]

sumsOf' :: [Int] -> Int  -> [Int]
sumsOf' _ 0 = []
sumsOf' n k = (sumsOf' n (k-1)) ++ (sumUntil k n)

sumsOf :: [Int] -> [Int]
sumsOf n = sumsOf' n (length n)

-- questao 06

squares :: Integer -> [Integer]
squares n = findSquares n 1 []

findSquares :: Integer -> Integer -> [Integer] -> [Integer]
findSquares 0 _ n = n
findSquares i k n = if equals  then findSquares (i-1) (k+1) (n ++ square) else findSquares i (k+1) n
            where equals = ((last . show) (k*k)) == ((head . show) (k*k))
                  square = [k * k]
