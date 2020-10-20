{--
   1. let: permite definição de funções no interpretador
      it:  resultado da última expressão avaliada

 
   2. square               :: Integer -> Integer
      double (square 2)    :: Num a => a
      square (double 2)    :: Num a => a
      23 - double (3+1)    :: Num a => a
      23 - double 3+1      :: Num a => a
      it + 34              :: Num a => a
      13 `div` 5           :: Num a => a
      13 `mod` 5           :: Num a => a


   3. erros
      double 2 3 , a função double aguarda 1 argumento e não 2
      double square , a função double aguarda 1 valor do tipo a que é uma instância de Num e não uma função
      2 não é uma função


   4. quadradoDobro :: Int -> Int
      quadradoDobro x = double ( square x)

      dobroQuadrado :: Int -> Int
      dobroQuadrado x = square (double x)

   5. 
      3.9 threeDifferent :: Integer -> Integer -> Integer -> Bool
      o resultado é verdadeiro somente se todos os três valores
      forem diferentes. Qual é a resposta para 
      threeDifferent 3 4 3?

      threeDifferent :: Integer -> Integer -> Integer -> Bool
      threeDifferent a b c
       | (a == c) || (a == b) || (b == c) = False
       | otherwise = True


      3.10 Sobre a função fourEqual :: Integer -> Integer -> Integer -> Integer -> Bool
      que retorna o valor True apenas se todos os quatro 
      argumentos forem iguais. Dê uma definição de "fourEqual"
      modelada sobre a definição de "threeDifferent". Agora
      dê uma definição de "fourEqual" que usa a função 
      "threeDifferent" na sua definição

      fourEqual :: Integer -> Integer -> Integer -> Integer -> Bool
      fourEqual a b c d = threeDifferent a b c && 


      3.11
      3.13
      3.14
 --}
