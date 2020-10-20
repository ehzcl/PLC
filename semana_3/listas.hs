import Data.Char  -- to use toUpper

-- questao 01

paraMaiscula :: String -> String
paraMaiscula s = [ toUpper x | x <- s ]

paraMaiscula' :: String -> String
paraMaiscula' s = [ toUpper x | x <- s, x < '0' ||  x > '9']


-- questao 02

divisores :: Integer -> [Integer]
divisores n = [ x | x <- [1 .. n], n `mod` x == 0]

isPrime :: Integer -> Bool
isPrime n = if length (divisores n) == 2 then True else False


-- questao 03

menorLista :: [Integer] -> Integer
menorLista n 
 | length n == 1 = head n
 | otherwise = menorLista [ x | x <- n, x < head n]


-- questao 04

fibTable :: Integer -> IO()
fibTable n = putStr (fibHeader ++ 
             fibBody lista)
   where
      lista = fib n ([], [])

fibHeader :: String
fibHeader = "n\tfib n\n"

fibBody :: ([Integer], [Integer]) -> String
fibBody ([], []) = ""
fibBody (a, b) = show (last a) ++ "\t" ++ show (last b) ++ "\n" ++ fibBody (init a, init b)


fib :: Integer -> ([Integer], [Integer]) -> ([Integer], [Integer])
-- first is seq, second is fib (seq)
fib 0 l = (0:(fst l), 0:(snd l))
fib 1 l = (1:(fst (fib 0 l)), 1:(snd (fib 0 l)))
fib x l =
  let 
    la = fib (x-1) l
    fstl = fst la
    sndl = snd la
    a = head sndl
    b = head (tail sndl)
  in (x:fstl, (a + b):sndl)


-- questao 5


measure :: [a] -> Integer
measure [] = -1
measure b = if a == -1 then 1 else a + 1
  where
    a = measure (tail b)


-- questao 6


takeFinal :: Integer -> [a] -> [a]
takeFinal 1 a = (last a):[]
takeFinal n a = (last a):(takeFinal (n-1) (init a))


-- questao 7


remove :: Int -> [a] -> [a]
remove n a = [ a!!(x) | x  <- [0 .. ((length a) -1 )], x /= n]


-- questao 8

firstInc :: [Integer] -> Integer
firstInc [] = 0
firstInc (a:hs) = a + 1
-- firstInc a = if a == [] then 0 else (head a) + 1

-- questao 9

add2 :: [Integer] -> Integer
add2 [] = 0
add2 a = if tail a == [] then head a else head a + head (tail a)

-- questao 10


produto :: [Integer] -> Integer
produto [] = 1
produto n = (head n) * produto (tail n)


-- questao 11

unique :: [Integer] -> [Integer]
unique [] = []
unique (x:xs) = if inside xs x then unique [y | y <- xs, y /= x] else x:(unique xs)


inside :: [Integer] -> Integer -> Bool
inside (x:xs) n
 | x == n = True
 | xs == [] = False
 | otherwise = inside xs n

-- questao 12

lista_crescente :: [Integer] -> Bool
lista_crescente [] = True
lista_crescente (x:xs) = if [ y | y <- xs, y > x] == xs then True && (lista_crescente xs) else False
