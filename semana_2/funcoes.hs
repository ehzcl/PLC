-- questao 1

dobro :: Integer -> Integer
dobro x = x * x


-- questao 2

quadruplo :: Integer -> Integer
quadruplo x = dobro x + dobro x


-- questao 3


poli2 :: Double -> Double -> Double -> Double -> Double
poli2 a b c x = a * (x * x) + (b * x + c)


-- questao 4


parImpar :: Integer -> [Char]
parImpar x = if x `mod` 2 == 0 then "Par" else "Impar"


-- questao 5


maxThree :: Integer -> Integer -> Integer -> Integer
maxThree a b c 
 | a > c && a > b = a
 | b > c && b > a = b
 | c > a && c > b = c
 | otherwise = c


maxFour :: Integer -> Integer -> Integer -> Integer -> Integer
-- usando maxThree
{--

maxFour a b c d = maxThree a b (maxThree d c a)

--}

{-- usando max

maxFour a b c d = max a (max b (max c d))

--}

-- usando max e maxThree

maxFour a b c d = max a (maxThree b c d)


-- questao 6

quantosIguais :: Integer -> Integer -> Integer -> Integer
quantosIguais a b c
 | x == y && x > 0= 3
 | otherwise = max x y
 where
   x = if a == b then 2 else if a == c then 2 else 0
   y = if b == c then 2 else 0


-- questao 7


ehZero :: Integer -> Bool
ehZero 0 = True
ehZero _ = False


-- questao 8


sumTo :: Integer -> Integer
sumTo n
 | n == 1 = 1
 | otherwise = sumTo (n-1) + n


-- questao 9


potencia n k
 | k == 1 = n
 | otherwise = potencia n (k-1) * n


-- questao 10


binom _ 0 = 1
binom 0 k = 0 -- Se chegarmos aqui eh por que k != 0
binom n k = bn_1 + bnk_1
 where
   bn_1 = binom (n-1) k
   bnk_1 = binom (n-1) (k-1)


-- questao 11

tribonacci :: Integer -> Integer
tribonacci 1 = 1
tribonacci 2 = 1
tribonacci n = tribonacci' 1 1 2 (n-2)


tribonacci' :: Integer -> Integer -> Integer -> Integer -> Integer
tribonacci' a b c 1 = c
tribonacci' a b c n = tribonacci' b c (c+a+b) (n-1)


-- questao 12


addEspacos :: Int -> String
addEspacos n
 | n == 0 = ""
 | otherwise = addEspacos (n-1) ++ " "


-- questao 13


paraDireita :: Int -> String -> String
paraDireita n s = addEspacos n ++ s


-- questao 14


vendas 0 = 1
vendas 1 = 5
vendas 2 = 6
vendas 3 = 7


totalVendas :: Int -> Int
totalVendas 0 = 0
totalVendas n = totalVendas (n-1) + vendas n

imprimeTotal :: Int -> String
imprimeTotal n = paraDireita 4 ("Total"
                             ++ addEspacos 4
                             ++ show (totalVendas n)
                             ++ "\n")


imprimeMedia :: Int -> String
imprimeMedia n = paraDireita 4 ("Media" ++ addEspacos 4 ++ show ((fromIntegral (totalVendas n)) / (fromIntegral (n+1))) ++ "\n")

imprimeCabecalho = addEspacos 3 ++ "Semana" ++ addEspacos 3 ++ "Venda\n"

imprimeSemanas :: Int -> String
imprimeSemanas 0 = addEspacos 6 ++ "0" ++ addEspacos 7 ++ show (vendas 0) ++ "\n"
imprimeSemanas n = imprimeSemanas (n-1) ++ addEspacos 6 ++ show (n) ++ addEspacos 7 ++ show (vendas n) ++ "\n"


imprimeTabela :: Int -> String
imprimeTabela n =   imprimeCabecalho
                 ++ imprimeSemanas n
                 ++ imprimeTotal n
                 ++ imprimeMedia n



impressao :: Int -> IO()
impressao n = putStr (imprimeTabela n)
