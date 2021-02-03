import Data.Char
--a
--[6,12,18]

--[x | x <- [1..20], mod x 6 == 0]
-- vai aos números de 1 a 20 e pega nos divisíveis por 2 e por 3 ao mm tempo

--b
--[6,12,18]
-- vai aos números pares de 0 a 20 e pega nos divisíveis por 3

--c
-- [(10,20),(11,19),(12,18),(13,17),(14,16),(15,15),(16,14),(17,13),(18,12),(19,11),(20,10)]
-- vai aos números de 1 a 20 e pega em pares cuja soma do x com o y dê 30 

--d 
-- sum [1,3,5,7,9] = 25
-- faz a soma dos elementos da lista de elementos ímpares de 1 a 10

--2
--a
-- [2^x | x <- [0..10]]

--b
-- [x | x <- [1..5], y <- [1..5] , x+y == 6] 

--c
-- [[x|x<-[1..y]] | y <- [1..5]]

--d
-- [replicate x 1 | x <- [1..5]]

--OU 

-- [[1 | x <- [1..y]] | y <- [1..5]]

--e
-- [factorial x | x <- [1..6]]
--    where factorial 0 = 1
--          factorial x = x * factorial (x - 1)
--OU

-- [product [y | y <- [1..x]] | x <- [1..6]]

--3
digitAlpha :: String -> (String,String)
digitAlpha (x:xs) | isDigit x == True = (a,x:b)
                  | isAlpha x == True = (x:a,b)
                  | otherwise = (a,b)
    where (a,b) = digitAlpha (xs)
digitAlpha _ = ("","")

--4
nzp :: [Int] -> (Int,Int,Int)
nzp (x:xs) | x < 0 = (1+a,b,c)
           | x == 0 = (a,1+b,c)
           | x > 0 = (a,b,1+c)
    where (a,b,c) = nzp xs
nzp _ = (0,0,0)

--5 --- ???????????
divMod' :: Integral a => a -> a -> (a,a)
divMod' a 0 = error "Divisão por 0"
divMod' a b
    |a<0 && b>0 || a>0 && b<0 = (-res,resp)
    |(a-b)<=0 = (x,(y+a))
    |otherwise = ((x+1),y)
    where (x,y) = (divMod' (a-b) (b) )
          (res,resp) = (divMod' (abs a) (abs b))

--6
fromDigits :: [Int] -> Int
fromDigits [] = 0 
fromDigits l = acumula ((length l)-1) l 

acumula :: Int -> [Int] -> Int
acumula acc [] = 0
acumula acc (x:xs) = (10^acc) * x + acumula (acc-1) xs


--8 
fib :: Int -> Int
fib n = acc n (0,1)
    where 
        acc 0 (a,c) = a
        acc 1 (a,c) = c
        acc n (a,c) = acc (n-1) (c,a+c)


