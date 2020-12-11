enumFromTo'' :: Int -> Int -> [Int]
enumFromTo'' x y | x == y = [x]
                 | x < y = x : enumFromTo'' (x+1) y
                 | otherwise = []

efto :: Int -> Int -> Int -> [Int]
efto a b c | a < b && a > c = []
           | a > b && a < c = []
           | otherwise = a : efto b (2*b-a) c

junta :: [a] -> [a] -> [a]
junta l [] = l
junta [] l = l
junta (x:xs)(h:t) = x : junta xs (h:t)

posicao :: [a] -> Int -> a 
posicao (h:t) 0 = h
posicao (h:t) x = posicao t (x-1)

inverte :: [a] -> [a] 
inverte [] = []
inverte l = last l : inverte(init l)

pega :: Int -> [a] -> [a]
pega _ [] = []
pega 0 l = []
pega x (h:t) = h : pega (x-1) t

dropar :: Int -> [a] -> [a]
dropar _ [] = []
dropar 0 l = l
dropar n (h:t) = dropar (n-1) t

zip'' :: [a] -> [b] -> [(a,b)]
zip'' [] l = []
zip'' l [] = []
zip'' (x:xs)(y:ys) = (x,y) : zip'' xs ys

eleme :: Eq a => a -> [a] -> Bool
eleme _ [] = False
eleme x (h:t) = if x == h then True 
                else elem x t

replicar :: Int -> a -> [a]
replicar 0 _ = []
replicar x a = a : replicar (x-1) a

noMeio :: a -> [a] -> [a]
noMeio x [a] = [a]
noMeio x (h:t) = h : x : noMeio x t

grupa :: Eq a => [a] -> [[a]]
grupa [] = []
grupa (h:t) = takeWhile (==h) (h:t) : grupa (dropWhile (/=h) (h:t))

concatx :: [[a]] -> [a]
concatx [] = []
concatx (h:t) = h ++ concatx t

initio :: [a] -> [[a]] 
initio [] = [[]]
initio l = initio (init l) ++ [l]

tailss :: [a] -> [[a]]
tailss [] = [[]]
tailss (h:t) = (h:t) : tailss t

prefixo :: Eq a => [a] -> [a] -> Bool
prefixo [] _ = True
prefixo _ [] = False
prefixo (x:xs) (h:t) | x == h = prefixo xs t
                     | otherwise = False

sufixo :: Eq a => [a] -> [a] -> Bool
sufixo [] _ = True
sufixo _ [] =  False
sufixo l1 l2 = if last l1 == last l2 then sufixo (init l1)(init l2)
               else False

isso :: Eq a => [a] -> [a] -> Bool
isso [] [] = True
isso [] l = True
isso l [] = False
isso (h:t)(x:xs) | h == x = isso t xs
                 | otherwise = isso (h:t) xs 


elemIndicess :: Eq a => a -> [a] -> [Int]
elemIndicess x l = aux x l 0

aux :: Eq a => a -> [a] -> Int -> [Int]
aux _ [] n = []
aux x (h:t) n = if x == h then [n] ++ aux x t (n+1) 
                else aux x t (n+1)


nubb :: Eq a => [a] -> [a]
nubb [] = []
nubb (x:xs) | elem x xs = nubb xs
            | otherwise = x: nubb xs


deleteee :: Eq a => a -> [a] -> [a]
deleteee _ [] = []
deleteee x (h:t) = if x == h then t
                   else h : deleteee x t

barra :: Eq a => [a] -> [a] -> [a]
barra [] [] = []
barra l [] = l
barra [] l = []
barra l (x:xs) = barra (deleteee x l) xs


uniao :: Eq a => [a] -> [a] -> [a]
uniao l [] = l
uniao [] l = l
uniao (x:xs)(h:t) = if x == h then uniao (x:xs) t 
                    else uniao (x:xs) t ++ [h]

--uniao :: Eq a =>[a] -> [a] -> [a]
--uniao l [] = l
--uniao (x:xs) (h:t) = if elem h (x:xs) then uniao (x:xs) t
--                     else uniao ((x:xs) ++ [h]) t


interseta :: Eq a => [a] -> [a] -> [a]
interseta [] l = []
interseta l [] = []
interseta (x:xs)(h:t) = if elem x (h:t) then x : interseta xs (h:t)
                        else interseta xs (h:t)

insert' :: Ord a => a -> [a] -> [a]
insert' x [] = [x]
insert' x (h:t) = if x <= h then x : (h:t)
                 else h : insert' x t 

unwords' :: [String] -> String
unwords' [] = []
unwords' (h:t) = h ++ " " ++ unwords' t

unlines' :: [String] -> String
unlines' [] = []
unlines' (h:t) = h ++ "\n" ++ unlines' t

pMaior' :: Ord a => [a] -> Int
pMaior' [x] = 0
pMaior' (h:t) = if h == aux (h:t) then 0
               else 1 + pMaior' t
    where  aux [x] = x
           aux (x:y:xs) = if x > y then aux (x:xs)
                          else aux (y:xs)

temRepetidos :: Eq a => [a] -> Bool
temRepetidos [] =  False
temRepetidos (x:xs) = if elem x xs then True
                      else temRepetidos xs

algarismos :: [Char] -> [Char]
algarismos [] = []
algarismos (h:t) = if ((h >= '0') && (h <= '9')) then h: algarismos t
                   else algarismos t

posImpares :: [a] -> [a]
posImpares [] = []
posImpares [x] = []
posImpares (x:y:ys) = y : posImpares ys

posPares :: [a] -> [a]
posPares [] = []
posPares [x] = [x]
posPares (x:y:ys) = x : posPares ys

isSorted :: Ord a => [a] -> Bool
isSorted [] = True
isSorted [x] = True
isSorted (x:y:ys) = if (x <= y) then isSorted (y:ys)
                    else False

iSort :: Ord a => [a] -> [a]
iSort [] = []
iSort [x] = [x]
iSort (h:t) = insert h (iSort t)
    where insert x [] = [x]
          insert x (h:t) | x <= h = x : h : t
                         | otherwise = h: insert x t  

menor :: String -> String -> Bool
menor [] [] = True
menor [] l  = True
menor l []  = False
menor (h:t)(x:xs) | h > x = False
                  | h < x = True
                  | otherwise = menor t xs

elemSet :: Eq a => a -> [(a,Int)] -> Bool
elemSet _ [] = False
elemSet x ((c,cs):t) = if x == c then True
                       else elemSet x t

lengthSet :: [(a,Int)] -> Int
lengthSet [] = 0 
lengthSet ((c,cs):t) = cs + lengthSet t

converte :: [(a,Int)] -> [a]
converte [] = []
converte ((c,1):t) = c : converte t
converte ((c,cs):t) = c : converte ((c,cs-1):t)

inserir :: Eq a => a -> [(a,Int)] -> [(a,Int)]
inserir x [] = [(x,1)]
inserir x ((c,cs):t) = if x == c then ((c,cs+1):t)
                       else (c,cs) : inserir x t

remover :: Eq a => a -> [(a,Int)] -> [(a,Int)]
remover _ [] = []
remover x ((c,1):t) = if x == c then t
                  else (c,1) : remover x t
remover x ((c,cs):t) = if x == c then (c,cs-1):t
                       else (c,cs) : remover x t

constroi :: Ord a => [a] -> [(a,Int)]
constroi [] = []
constroi l = aux 1 l
    where aux i [x] = [(x,i)]
          aux i (x:y:xs) = if x == y then aux (i+1) (x:xs)
                           else (x,i) : aux 1 (y:xs)

partitionEithers' :: [Either a b] -> ([a],[b])
partitionEithers' [] = ([],[])
partitionEithers' (h:t) = case h of
                          Left a -> ((a:x),y)
                          Right b -> (x,(b:y))
  where (x,y) = partitionEithers' t

catMaybes' :: [Maybe a] -> [a]
catMaybes' [] = []
catMaybes' (h:t) = case h of
                   Just x -> x:catMaybes' t
                   Nothing -> catMaybes' t

data Movimento = Norte | Sul | Este | Oeste 
               deriving Show

posicao' :: (Int,Int) -> [Movimento] -> (Int,Int)
posicao' (x,y) [] = (x,y)
posicao' (x,y)(Norte:xs) = posicao' (x,y+1) xs
posicao' (x,y)(Sul:xs) = posicao' (x,y-1) xs
posicao' (x,y)(Este:xs) = posicao' (x+1,y) xs
posicao' (x,y)(Oeste:xs) = posicao' (x-1,y) xs

caminho :: (Int,Int) -> (Int,Int) -> [Movimento]
caminho (x,y) (z,w) | (x == z && y == w) = []
                    | x > z = Oeste : caminho (x-1,y) (z,w)
                    | x < z = Este : caminho  (x+1,y) (z,w)
                    | y < w = Norte : caminho (x,y+1) (z,w)
                    | otherwise = Sul : caminho (x,y-1) (z,w)


vertical :: [Movimento] -> Bool
vertical [] = True
vertical (h:t) = case h of
                 Norte -> vertical t
                 Sul -> vertical t
                 _ -> False

data Posicao = Pos Int Int
             deriving Show

maisCentral :: [Posicao] -> Posicao 
maisCentral [Pos x y] = Pos x y
maisCentral ((Pos x y):(Pos z w):xs) = if (dist' (x,y) > dist' (z,w)) then maisCentral ((Pos z w):xs)
                                       else maisCentral ((Pos x y):xs)
dist' :: (Int,Int) -> Float
dist' (x,y) = sqrt (fromIntegral(x^2 + y^2))

vizinhos :: Posicao -> [Posicao] -> [Posicao]
vizinhos _ [] = []
vizinhos (Pos x y) ((Pos a b):t) = if ((x==a+1) || (x==a-1) || (y==b+1) || (y==b-1)) then (Pos a b) : vizinhos (Pos x y) t
                                   else vizinhos (Pos x y) t

data Semaforo = Verde | Amarelo | Vermelho
              deriving Show

intersecaoOk :: [Semaforo] -> Bool
intersecaoOk [] = False
intersecaoOk l = if contaNaoVermelhos l <= 1 then True
                     else False

contaNaoVermelhos :: [Semaforo] -> Int
contaNaoVermelhos [] = 0 
contaNaoVermelhos (h:t) = case h of 
                          Verde -> 1 + contaNaoVermelhos t
                          Amarelo -> 1 + contaNaoVermelhos t
                          Vermelho -> contaNaoVermelhos t