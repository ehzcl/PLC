-- questao 1
maior3 :: Integer -> Integer -> Integer -> Integer
maior3 a b c = max a (max b c)

menor3 :: Integer -> Integer -> Integer -> Integer
menor3 a b c = min a (min b c)

menorMaior :: Integer -> Integer -> Integer -> (Integer, Integer)
menorMaior a b c = ((menor3 a b c), (maior3 a b c))

-- questao 2

type TriplaI = (Integer, Integer, Integer)

ordenaTripla :: TriplaI -> TriplaI
ordenaTripla (a, b, c) = (y, z, x)
  where
      x = maior3 a b c
      y = menor3 a b c
      z = if a /= x && a /= y then a else if b /= x && b /= y then b else c

-- questao 3
--
type Ponto = (Float, Float)
type Reta = (Ponto, Ponto)

xP :: Ponto -> Float
xP a = fst a

yP :: Ponto -> Float
yP (_, b) = b

ehVertical :: Reta -> Bool
ehVertical (a, b) = (xP a) == (xP b)

-- questao 4

yDaReta :: Float -> Reta -> Float
yDaReta n (p1, p2) = yP p1 + ((n - xP p1) * ((yP p2 - yP p1)/(xP p2 - xP p1) ))

