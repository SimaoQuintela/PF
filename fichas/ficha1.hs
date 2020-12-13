module Ficha1 where
--ficha1
import Data.Char
--a
perimetro :: Double -> Double
perimetro r = 2 * pi * r

--b
distancia :: (Double,Double) -> (Double,Double) -> Double
distancia (x,y) (a,b) = sqrt ((a-x)^2 + (b-y)^2)

--c
primUlt :: [a] -> (a,a)
primUlt (x:xs) = (x,last (x:xs))

--d
multiplo :: Int -> Int -> Bool
multiplo x y = if ((mod x y) == 0) then True 
               else False

--e
truncaImpar :: [a] -> [a]
truncaImpar [] = []
truncaImpar (x:xs) = if (mod (length (x:xs)) 2) == 0 then x:xs
                     else xs
--f
max2 :: Int -> Int -> Int
max2 x y = if x >= y then x 
           else y
--g
max3 :: Int -> Int -> Int -> Int
max3 x y z = max2 (max2 x y) z

--2
--a
nRaizes :: Float -> Float -> Float -> Int
nRaizes a b c | delta > 0 = 2 
              | delta == 0 = 1
              | otherwise = 0
    where delta = sqrt (b^2 -4 *a*c)

--b
raizes :: Float -> Float -> Float -> [Float]
raizes a b c | nRaizes a b c == 2 = [r1,r2]
             | nRaizes a b c == 1 = [r1]
             | otherwise = []
    where r1 = (-b + sqrt (b^2 -4 *a*c))/2*a
          r2 = (-b - sqrt (b^2 -4 *a*c))/2*a

--3
--a
type Hora = (Int,Int)

valida :: Hora -> Bool
valida (x,y) = if (x >= 0 && x < 24) && (y >= 0 && y <= 60) then True
               else False
--b
comparaHoras :: Hora -> Hora -> Bool
comparaHoras (x,y) (a,b) | x > a = True
                         | x < a = False
                         | (x == a && y > b) = True
                         | otherwise = False

--c
hora2min :: Hora -> Int
hora2min (x,y) = 60*x + y

--d
min2hora :: Int -> Hora
min2hora x | x < 60 = (0,x)
           | otherwise = (div x 60 , mod x 60)

--e
diferenca :: Hora -> Hora -> Int
diferenca x y = abs (hora2min(x) - hora2min(y))

--f 
adicionaMin :: Int -> Hora -> Hora
adicionaMin x y = min2hora ((hora2min y) + x)


--4
data Hora2 = H Int Int 
             deriving (Show)
--a
valida2 :: Hora2 -> Bool
valida2 (H x y) = if (x >= 0 && x < 24) && (y >= 0 && y <= 60) then True 
b
depois :: Hora2 -> Hora2 -> Bool
depois (H x y) (H a b) | x > a = True
                       | x < a = False
                       | (x == a && y > b) = True
                       | otherwise = False
 
c
horaParaMin :: Hora2 -> Int
horaParaMin (H x y) = x*60 +y
 
d
minParaHora :: Int -> Hora2
minParaHora x | x < 60 = H 0 x 
              | otherwise = H (div x 60) (mod x 60)
 
e
diferencaHora :: Hora2 -> Hora2 -> Hora2
diferencaHora (H x y) (H a b) = minParaHora (abs (horaParaMin (H x y) - horaParaMin (H a b)))
 
f
adiciona :: Int -> Hora2 -> Hora2
adiciona x (H a b) = minParaHora (horaParaMin (H a b) + x)

--5
data Semaforo = Verde | Amarelo | Vermelho
                deriving (Show,Eq)

next :: Semaforo -> Semaforo
next x = case x of 
         Verde -> Amarelo
         Amarelo -> Vermelho
         Vermelho -> Verde

stop :: Semaforo -> Bool
stop x | x == Vermelho = True
       | otherwise = False

safe :: Semaforo -> Semaforo -> Bool
safe x y |  ((x == Vermelho) && (y == Amarelo)) = True
         |  ((x == Amarelo) && (y == Vermelho)) = True
         |  ((x == Verde) && (y == Vermelho)) = True
         |  ((x == Vermelho) && (y == Verde)) = True
         |  ((x == Vermelho) && (y == Vermelho)) = True
         |  otherwise = False

--6
data Ponto = Cartesiano Double Double | Polar Double Double
             deriving (Show,Eq)
--a
posx :: Ponto -> Double 
posx (Cartesiano x y) = x 
posx (Polar d a) = d * cos a

--b
posy :: Ponto -> Double
posy (Cartesiano x y) = y
posy (Polar d a) = d * sin a

--c
raio :: Ponto -> Double
raio (Cartesiano x y) = sqrt (x^2 + y^2)
raio (Polar d a) = d 

--d
angulo :: Ponto -> Double
angulo (Cartesiano x y) = atan (y/x)
angulo (Polar d a) = a

--e
dist :: Ponto -> Ponto -> Double
dist (Cartesiano x y) (Cartesiano a b) = sqrt ((a-x)^2 + (b-y)^2)
dist p1 p2 = sqrt ((x-a)^2 + (y-b)^2)
    where (Cartesiano x y) = polar2cartesiano p1
          (Cartesiano a b) = polar2cartesiano p2

cartesiano2polar (Cartesiano x y) = Polar (raio (Cartesiano x y)) (atan (y/x))
cartesiano2polar (Polar d a) = Polar d a

polar2cartesiano (Polar d a) = Cartesiano ((sin a) * d) ((cos a)*d)
polar2cartesiano (Cartesiano x y) = Cartesiano x y 

--7
--a
data Figura = Circulo Ponto Double
            | Retangulo Ponto Ponto 
            | Triangulo Ponto Ponto Ponto 
              deriving (Show,Eq)

--a
poligono :: Figura -> Bool
poligono x = case x of
             Circulo _ _ -> False
             _ -> True

--b 
--vertices :: Figura -> [Ponto]
--vertices f = case f of
--           Triangulo p1 p2 p3 -> [p1,p2,p3]
--           Retangulo p1 p2 -> [p1,p2,p3,p4] 
--  where p3 = Cartesiano (posx p1) (posy p2)
--        p4 = Cartesiano (posx p2) (posy p1)
--           _ -> []

--c
area :: Figura -> Double
area (Triangulo p1 p2 p3) =
       let a = dist p1 p2 
           b = dist p2 p3
           c = dist p3 p1
           s = (a+b+c) / 2
       in sqrt (s*(s-a)*(s-b)*(s-c))

area (Circulo p r) = pi * r^2
area (Retangulo p1 p2) = abs (posx p2 - posx p1) * abs (posy p2 - posy p1) 

--d
perimetro' :: Figura -> Double
perimetro' (Circulo _ r) = 2 * pi * r
perimetro' (Triangulo p1 p2 p3) = (dist p1 p2) + (dist p1 p3) + (dist p2 p3)
perimetro' (Retangulo p1 p2) =
       let p3 = Cartesiano (posx p1) (posy p2)
           base = dist p2 p3
           altura = dist p1 p3
       in 2 * base * altura

--8 
--a
isLower' :: Char -> Bool
isLower' x = if ord x >= 97 && ord x <= 122 then True
            else False

--b
isDigit' :: Char -> Bool
isDigit' x = if ord x >= 48 && ord x <= 57 then True
             else False

--c
isAlpha' :: Char -> Bool
isAlpha' x = if ord x >= 62 && ord x <= 122 then True
             else False

--d
toUpper' :: Char -> Char
toUpper' x = chr ((ord x)-32)

--e
intToDigit :: Int -> Char
intToDigit d = chr (d+48)

--f
digitToInt :: Char -> Int
digitToInt c = (ord c) -48
