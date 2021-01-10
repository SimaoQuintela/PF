import Ficha2
--import Data.List
--1
--a
any' :: (a-> Bool) -> [a] -> Bool
any' f [] = False
any' f (x:xs) | f (x) == True = True
              | otherwise = any' f xs

--b
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f l [] = []
zipWith' f [] l = []
zipWith' f (x:xs) (h:t) = (f (x) (h)) : zipWith' f (xs) (t)

--c
takeWhile' :: (a->Bool) -> [a] -> [a]
takeWhile' f [] = []
takeWhile' f (x:xs) | f x == True = x : takeWhile' f xs
                    | otherwise = []

--d
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' f [] = []
dropWhile' f (x:xs) | f x == True = dropWhile' f xs
                    | otherwise = (x:xs)
--e
span' :: (a->Bool) -> [a] -> ([a],[a])
span' f [] = ([],[])
span' f (h:t) | f h = (h:x,y)
              | otherwise = ([],h:t)
              where (x,y) = span' f t
--f
deleteBy' :: (a -> a -> Bool) -> a -> [a] -> [a]
deleteBy' _ _ [] = []
deleteBy' f x (h:t) | f x h = t
                   | otherwise = h : deleteBy' f x t

--g
sortOn' :: Ord b => (a->b) -> [a] -> [a]
sortOn' f [] = []
sortOn' f (x:xs) = inserir x (sortOn' f xs)
    where inserir x [] = [x]
          inserir x (h:t) = if f x > f h then h : inserir x t
                            else x:h:t
--2
--type Polinomio1 = [Monomio]
--type Monomio1 = (Float,Int)

--a
selgrau' :: Int -> Polinomio -> Polinomio
selgrau' x l = filter (\n -> snd n == x) l
--b
conta' :: Int -> Polinomio -> Int
conta' x l = length $ filter(\n -> snd n == x) l
--c
grau' :: Polinomio -> Int
grau' l = foldl (\acc x -> if acc > snd x then acc else snd x) 0 l
--d
deriv' :: Polinomio -> Polinomio
deriv' l = filter (/= (0,0)) $ map (\(c,g) -> if g > 0 then ((c * fromIntegral g),g-1) else (0,0)) l
--e
calcula' :: Float -> Polinomio -> Float
calcula' x l = foldl (\acc (c,g) -> acc + c * (x^g)) 0 l
--f
simp' :: Polinomio -> Polinomio
simp' l = filter (\x -> fst x /= 0) l
--g
mult' :: Monomio -> Polinomio -> Polinomio
mult' (c,g) = map (\(c2,g2) -> (c*c2,g+g2))
--h
--ordena' :: Polinomio -> Polinomio
--ordena' l = foldr (\x -> insert x) [] l

--i  -- muita dificuldade neste
{-normaliza :: Polinomio -> Polinomio
normaliza [a] = [a]
normaliza [] = []
normaliza (h:t) = foldl (\((c,e):p) (c',e')-> if e==e'
	                                     then (c+c',e):t
	                                     else ((c',e')(c,e):t)) h t
-}
--j
{-
soma :: Polinomio -> Polinomio -> Polinomio
soma p1 p2 = normaliza $ (++) p1 p2
-}

--k
produto' :: Polinomio -> Polinomio -> Polinomio
produto' p1 p2 = foldl (\acc x -> soma (mult x p2) acc) [] p1

--l --- ?????????????
equiv' :: Polinomio -> Polinomio -> Bool
equiv' p1 p2 | normaliza' p1 == normaliza' p2 = True
             | otherwise = False


--3
type Mat a = [[a]]

--a
dimOK :: Mat a -> Bool
dimOK (l:ls) = all (\x -> length x == length l) ls

--b
dimMat :: Mat a -> (Int, Int)
dimMat [] = (0,0)
dimMat (h:t) = (length (h:t), length h)
--
-- dimMat :: Mat a -> (Int,Int)
-- dimMat [[]] = (0,0)
-- dimMat (x:m) | dimOk (x:m) = (length (x:m) , length x)
--              | otherwise = error "matriz mal definida"

--c
-- soma1 :: Num a => [a] -> [a] -> [a]
-- soma1 [] x = x
-- soma1 x [] = x
-- soma1 (h:t) (x:xs) = (h+x) : soma1 t xs

-- addMatriz :: Num a => Mat a -> Mat a -> Mat a
-- addMatriz x [[]] = x
-- addMatriz [[]] x = x
-- addMatriz (x:xs) (h:t) | (dimMat (x:xs) == dimMat (h:t)) = (soma1 x h : addMatriz xs t) 
--                        | otherwise = error "impossivel"

-------------------------c
addMatriz2 :: Num a => Mat a -> Mat a -> Mat a
addMatriz2 [] x = x
addMatriz2 (h:t) (x:xs) | dimMat (h:t) == dimMat (x:xs) = zipWith (+) h x : addMatriz2 t xs
                        | otherwise = error "Impossivel"

--d
transpose :: Mat a -> Mat a
transpose ([]:_) = []
transpose m = let l = map head m
                  rm = map tail m 
              in l : transpose rm

--e reparar isto , está mal para matrizes de ordem distinta
multMat :: Num a => Mat a -> Mat a -> Mat a 
multMat m1 m2 = if length (head m1) == length m2 then multiplica m1 (transpose m2) 
                else error "impossivel realizar esta multiplicacao"
multiplica :: Num a => Mat a -> Mat a -> Mat a
multiplica [] m = []
multiplica m [] = []
multiplica (h:t) (x:xs) = (zipWith (*) h x) : (multiplica t xs) 
-------------------
multMat' :: Num a => Mat a -> Mat a -> Mat a
multMat' m1 m2 = zipWith (\l1 l2 -> zipWith (+) l1 l2) m1 m2 

--f
zipWMat :: (a->b->c) -> Mat a -> Mat b -> Mat c 
zipWMat f m1 m2 = zipWith (\l1 l2 -> zipWith f l1 l2) m1 m2 

--g
--triSup :: Num a => Mat a -> Bool 
triSup [] =  True 
triSup (h:t) = let l = map head t
                   rm = map tail t 
                in (all (==0) l) && triSup rm 

rotateLeft :: Mat a -> Mat a
rotateLeft [] = []
rotateLeft m = let l = map last m
                   rm = map init m
               in l : rotateLeft rm  
