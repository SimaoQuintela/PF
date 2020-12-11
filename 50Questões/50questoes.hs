--1
enumFromTo' :: Int -> Int -> [Int]
enumFromTo' x y   | (x == y) = [x]
                  | (x < y) = x : enumFromTo' (x+1)y
                  | otherwise = []
--2
--eft :: Int -> Int -> Int -> [Int]
--eft x y z  |(x > y && y < z) = []
--           |(x < y && y > z) = [x]
--           |(x < y && y < z) = x : eft (y+1-x)z
--           |(x > y && y > z) = x : eft (y+1-x)z


--                       1 3 10  
--                       [1] enumFromThenTo (3 5 10)
--                       [1,3] enumFromThenTo (5 7 10)
--                       [1,3,5] enumFromThenTo (7 9 10)
--                       [1,3,5,7] enumFromThenTo (9 10)
--                       [1,3,5,7,9] enumFromThenTo (10)
--                       [1,3,5,7,9]


--3
conc :: [a] -> [a] -> [a]
conc [] l = l
conc l [] = l
conc (h:t) l = h:(conc t l)
--      
--      [1,2,3] conc [4,5,6]
--      [1] conc [2,3]

--4
pos :: [a] -> Int -> a
pos (h:t) 0 = h
pos (h:t) x = pos t (x-1) 


--5
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (h:t) = reverse' t ++ [h] 
--reverse' [1,2,3]
--reverse' [2,3] ++ [1]
--reverse' [3] ++ [2,1]
--reverse' [] ++ [3,2,1]
--[3,2,1]

--6
teik :: Int -> [a] -> [a]
teik x [] = [] 
teik 0 (h:t) = []
teik x (h:t) = h : (teik (x-1) t)

--2 [1,2,3,4]
--[1] teik 1 [2,3,4]
--[1,2] teik 0 [3,4]
--[1,2,3] teik [4]

--7
meudrop :: Int -> [a] -> [a]
meudrop x [] = []
meudrop 0 (h:t) = (h:t)
meudrop x (h:t) = (meudrop (x-1) t)

--meudrop 2 [10,20,30,40]
--1 [20,30,40]
--0 [30,40]
--[30,40]

--8 
zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = [] 
zip' (h:t)(x:xs) = (h,x) : (zip' t xs)

--zip' [1,2,3,4] [2,3,4,5]
--[(1,2)] zip' [2,3,4] [3,4,5]
--[(1,2)(2,3)] zip' [3,4] [4,5]
--[(1,2)(2,3)(3,4)] zip' [4] [5]
--[(1,2)(2,3)(3,4)(4,5)] zip' [] []
--[(1,2)(2,3)(3,4)(4,5)] 

--9
elem' :: Eq a => a -> [a] -> Bool
elem' x [] = False
elem' x (h:t) = if x == h then True else elem' x t

--10
replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = x : (replicate' (n-1) x)

--11
intersperse' :: a -> [a] -> [a]
intersperse' x [] = []
intersperse' x [h] = [h]
intersperse' x (h:t) = h : x : (intersperse' x t)

--12
group' :: Eq a => [a] -> [[a]]
group' [] = []
group' (h:t) = takeWhile (==h) (h:t) : group' (dropWhile (==h) (h:t))

--13
concat' :: [[a]] -> [a]
concat' [] = []
concat' (h:t) = h ++ concat' t


--14
inits' :: [a] -> [[a]]
inits' [] = [[]]
inits' l = (inits' [] ++ [l])

--15
tails' :: [a] -> [[a]]
tails' [] = [[]]
tails' (h:t) = (h:t) : tails' t

--16
ipf :: Eq a => [a] -> [a] -> Bool
ipf [] _ = True
ipf _ [] = False
ipf (h:t) (x:xs) = if h == x then ipf t xs else False


--17
isf :: Eq a => [a] -> [a] -> Bool
isf [] l = False
isf [w] [z] = True
isf l1 l2 = if ipf (reverse l1) (reverse l2) then True else False

--18
ordem :: Eq a => [a] -> [a] -> Bool
ordem [] [] = True
ordem [] _ = True 
ordem _ [] = False 
ordem (x:xs)(h:t) = if x==h then (ordem xs t) 
                    else (ordem (x:xs) t)

--19
--indice :: Eq a => a -> [a] -> Int -> Int
indice x l = indiceAux x l 0

indiceAux :: Eq a => a -> [a] -> Int -> [Int]
indiceAux _ [] n = []
indiceAux x (h:t) n = if x==h then [n] ++ indiceAux x t (n+1)
                      else indiceAux x t (n+1)

--20
nubb :: Eq a => [a] -> [a]
nubb [] = []
nubb (h:t) = if elem h t then nubb t
             else h : nubb t

--21
apaga :: Eq a => a -> [a] -> [a]
apaga _ [] = []
apaga x (h:t) = if x==h then t
                else h: apaga x t

--22
barra :: Eq a => [a] -> [a] -> [a]
barra l [] = l 
barra [] l = []
barra l (h:t) = barra (apaga h l) t


--23
union' :: Eq a => [a] -> [a] -> [a]
union' l [] = l
union' l (h:t) | unionelem h l = union' l t
               | otherwise = union' (l ++ [h]) t

unionelem :: Eq a => a -> [a] -> Bool
unionelem a [] = False
unionelem a (h:t) | a == h = True
                  | otherwise = unionelem a t
--24
interseta :: Eq a => [a] -> [a] -> [a]
interseta [] _ = []
interseta _ [] = []
interseta (x:xs)(h:t) = if x == h then x : interseta xs (h:t)
                        else interseta xs t

--25
inserir :: Ord a => a -> [a] -> [a]
inserir x [] = [x]
inserir x (h:t) = if x >= h then h: inserir x t
                  else x:(h:t)

--26
juntar :: [String] -> String
juntar [] = []
juntar (h:t) = h ++ " " ++ juntar t

--27 
linhass :: [String] -> String
linhass [] = []
linhass (h:t) = h ++ "\n" ++ linhass t

--28
sel :: [a] -> Int -> a -- existe em haskell e chama-se !!
-- sel ['a','b','c'] 0 - 'a'
sel [] p = error "index too large"
sel (h:t) p = if p == 0 then h 
              else sel t (p-1)

pmaior :: Ord a => [a] -> Int
pmaior [] = error "nao esta definido para listas vazias"
pmaior [x] = 0
pmaior (h:t) = if h > (sel t p) then 0
               else p+1
    where p = pmaior t
----------------- OU -----------------------------
--nos tipos      restricoes => tipo 
-- Ord a => Significa que se pode usar a comparação com maiores e menores
--pmaior l = snd (pmaiorAux l)

-------------------------------

pmaiorAux :: Ord a => [a] -> (a,Int)
pmaiorAux [x] = (x,0)
pmaiorAux (h:t) = if h > m then (h,0) else (m,p+1)
    where (m,p) = pmaiorAux t

--------------- REPETIR O P MAIOR-----------------------

--28
pMaior :: Ord a => [a] -> Int
pMaior [] = error "não existe"
pMaior [x] = 0 
pMaior (h:t) = if   h > (sel t p) then 0
               else p+1
    where p = pMaior t

sel :: [a] -> Int -> a
sel [] p = error "index too large"
sel (h:t) p = if p == 0 then h 
              else sel t (p-1)

--29
temRepetidos :: Eq a => [a] -> Bool
temRepetidos [] = False
temRepetidos [x] = False
temRepetidos (x:y:xs) | x == y = True
                      | elem x xs = True
                      | elem y xs = True 
                      | otherwise = temRepetidos xs

--30 
algarismos :: [Char] -> [Char]
algarismos [] = []
algarismos (x:xs) = if x >= '0' && x <= '9' then (x : algarismos xs)
                    else algarismos xs

--31
posImpares :: [a] -> [a]
posImpares []  = []
posImpares [x] = []
posImpares (x:y:xs) = y : posImpares xs

--32 
posPares :: [a] -> [a]
posPares [] = []
posPares [x] = [x]
posPares (x:y:xs) = x : posPares xs

--33
isSorted :: Ord a => [a] -> Bool
isSorted [] = True
isSorted [x] = True 
isSorted (x:y:xs) = if x <= y then isSorted (y:xs)
                  else False

--34
iSort :: Ord a => [a] -> [a]
iSort [] = []
iSort (x:xs) = insert x (iSort xs)

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (h:t) = if x <= h then x : h : t
                 else h : insert x t

--35
menor :: String -> String -> Bool
menor [] [] = False
menor [] [x] = True
menor [x] [] = False 
menor (x:xs)(h:t) | x < h = True
                  | x > h = False
                  | otherwise = menor xs t

--36
elemSet :: Eq a => a -> [(a,Int)] -> Bool
elemSet x [] = False
elemSet x ((a,b):t) = if x == a then True
                      else elemSet x t

--37
lms :: [(a,Int)] -> Int
lms [] = 0
lms ((a,b):t) = b + lms t

--38 
convert :: [(a,Int)] -> [a]
convert [] = []
convert ((a,1):t) = a : convert t
convert ((a,b):t) = a : convert ((a,b-1) : t)

--39
inseremset :: Eq a => a -> [(a,Int)] -> [(a,Int)]
inseremset x [] = [(x,1)]
inseremset x ((a,b):t) = if x == a then (a,b+1) : t
                         else (a,b) : inseremset x t

--40
removeconjunto :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeconjunto x ((a,b):t)       | (x == a && b == 1) = t
                                 | x == a && b >= 1 = (a,b-1) : t
                                 | otherwise = (a,b) : removeconjunto x t

--41
bob :: Ord a => [a] -> [(a,Int)]
bob [] = []
bob l = aux 1 l 
    where aux i [x] = [(x,i)]
          aux i (x:y:xs) = if x == y then aux (i+1)(x:xs)
                           else (x,i) : aux 1 (y:xs)

--42
--partitionEithers :: [Either a b] -> ([a],[b])
--partitionEithers [] = ([],[])
--partitionEithers (h:t) = case h of 
--                         Left a -> (a:partitionEithers t)
--                         Right b -> 

--43
catMaybe :: [Maybe a] -> [a]
catMaybe [] = []
catMaybe ((Just x) : xs)  = x : catMaybe xs
catMaybe ((Nothing) : xs) = catMaybe xs

--44
data Movimento = Norte | Sul | Este | Oeste
               deriving Show

posicao :: (Int,Int) -> [Movimento] -> (Int,Int)
posicao (x,y) [] = (x,y)
posicao (x,y) (Norte : xs) = posicao (x,y+1) xs
posicao (x,y) (Sul : xs) = posicao (x, y-1) xs
posicao (x,y) (Este : xs) = posicao (x+1,y) xs
posicao (x,y) (Oeste : xs) = posicao (x-1,y) xs

--45
caminho :: (Int,Int) -> (Int,Int) -> [Movimento]
caminho (x,y) (h,t) | ((x==h) && (y==t)) = []
                    | x > h = Oeste : caminho (x-1,y) (h,t)
                    | x < h = Este : caminho (x+1,y) (h,t)
                    | y < t = Sul : caminho (x,y-1) (h,t)
                    | otherwise = Norte : caminho (x,y+1) (h,t)
                    

--46
vertical :: [Movimento] -> Bool
vertical [] = False
vertical (x:xs) = case x of
                  Norte -> vertical xs 
                  Sul -> vertical xs
                  _ -> False

--47
data Posicao = Pos Int Int
               deriving Show

maisCentral :: [Posicao] -> Posicao
maisCentral [x] = x
maisCentral ((Pos x xs):(Pos y ys):t) | dist (x,xs) > dist (y,ys) = maisCentral ((Pos y ys):t)
                                       | otherwise = maisCentral ((Pos x,xs):t)

dist:: (Int,Int)-> Float
dist (x,y)= sqrt (fromIntegral ((x^2)+(y^2)))

--48
vizinhos :: Posicao -> [Posicao] -> [Posicao]
vizinhos x [] = []
vizinhos (Pos x y ((Pos w z):xs) | (x == (w-1)) || (x == (w+1)) || (x == (z-1)) || (x == (z+1)) = (Pos w z) : (vizinhos (Pos (x,y) xs)
                                 | otherwise = vizinhos (Pos x y) xs 

--49
mesmaOrdenada :: [Posicao] -> Bool
mesmaOrdenada [] = True
mesmaOrdenada [x] = True
mesmaOrdenada ((Pos x y) : (Pos w z) : t) = if y == z then mesmaOrdenada ((Pos x y) : t)
                                            else False

--50
----------