import Data.Char


-- questao 01

f :: a -> Integer
f _ = 1

length' :: [a] -> Integer
length' x = sum (map f x)


-- questao 02

-- a)

upper :: String -> String
upper x = map toUpper x

-- b)

dobro :: Int -> Int
dobro x = x + x

double :: [Int] -> [Int]
double x = map dobro x

-- c)

centavoToReal :: Int -> Float
centavoToReal x = reais + centavos
  where
    reais = fromIntegral (x `div` 100)
    centavos = (fromIntegral (x `mod` 100)) * 0.01

centavosReais :: [Int] -> [Float]
centavosReais x = map centavoToReal x


-- questao 03

-- a)

letras :: String -> String
letras [] = []
-- letras (x:xs) = if isDigit x then letras xs else x:(letras xs)
letras n = filter (not . isDigit) n

-- b)

rmChar :: Char -> String -> String
rmChar y [] = []
-- rmChar y (x:xs) = if y == x then rmChar y xs else x:(rmChar y xs)
rmChar y n = filter (/= y) n

-- c)

acima :: Int -> [Int] -> [Int]
-- acima n l = [ x | x <- l, x > n]
acima x n = filter (> x) n 

-- d)
desiguais :: [(Int, Int)] -> [(Int, Int)]
-- desiguais n = [ (x,s) | (x,s) <- n, x /= s]
desiguais n = filter (\(x,y) -> x /= y) n


-- questao 04

-- a)

lista1 s = map toUpper (filter isAlpha s)

-- b)

lista2 xs = map (\x -> 2 * x) (filter (> 3) xs)

-- c)

lista3 str = map reverse (filter (even . length) str)


-- questao 05

-- a)

productFold :: [Int] -> Int
--productFold [x] = x
--productFold (n:ns) = (productFold ns) * n

productFold n = foldr (*) 1 n

-- b)

andRec :: [Bool] -> Bool
andRec [x] = x
andRec (n:ns) = (andRec ns) && n
-- andRec n = foldr (&&) True n

--c)

concatRec :: [String] -> String
-- concatRec [x] = x
-- concatRec (n:ns) = n ++ concatRec ns

concatRec n = foldr (++) "" n
