import Data.Char

--2
--a
dobros :: [Float] -> [Float]
dobros [] = []
dobros (h:t) = 2 * h : dobros t

--b
numOcorre :: Char -> String -> Int
numOcorre _ [] = 0
numOcorre x (h:t) = if x == h then 1 + numOcorre x t
                    else numOcorre x t

--c
positivos :: [Int] -> Bool
positivos [] = True
positivos (h:t) = if h >= 0 then positivos t
                  else False

--d
soPos :: [Int] -> [Int]
soPos [] = []
soPos (x:xs) = if x >= 0 then x : soPos xs
               else soPos xs

--e
somaNeg :: [Int] -> Int
somaNeg [] = 0 
somaNeg (x:xs) = if x < 0 then x + somaNeg xs
                 else somaNeg xs

--f
tresUlt :: [a] -> [a]
tresUlt [] = []
tresUlt (x:xs) | length (x:xs) <= 3 = (x:xs)
               | otherwise = tresUlt xs

--g
segundos :: [(a,b)] -> [b]
segundos [] = []
segundos ((c,cs):t) = cs : segundos t

--h
nosPrimeiros :: (Eq a) => a -> [(a,b)] -> Bool
nosPrimeiros _ [] = False
nosPrimeiros x ((c,cs):t) = if x == c then True
                            else nosPrimeiros x t

--i 
sumTriplos :: (Num a, Num b, Num c) => [(a,b,c)] -> (a,b,c)
sumTriplos [(a,b,c)] = (a,b,c)
sumTriplos ((a,b,c):(d,e,f):t) = sumTriplos ((a+d,b+e,c+f):t)

--3
--a
soDigitos :: [Char] -> [Char]
soDigitos [] = []
soDigitos (x:xs) = if ord x >= 48 && ord x <= 57 then x : soDigitos xs
                   else soDigitos xs
--b
minusculas :: [Char] -> Int
minusculas [] = 0
minusculas (x:xs) = if ord x >= 97 && ord x <= 122 then 1 + minusculas xs
                    else minusculas xs
--c
nums :: String -> [Int]
nums [] = []
nums (x:xs) = if (ord x >= 48) && (ord x <= 57) then digitToInt x : nums xs
              else nums xs

type Polinomio = [Monomio]
type Monomio = (Float,Int)

--a
conta :: Int -> Polinomio -> Int
conta x [] = 0 
conta x ((c,g):t) = if x == g then 1 + conta x t
                    else conta x t 

--b
grau :: Polinomio -> Int
grau [(c,g)] = g 
grau ((c,g):(c2,g2):t) = if g >= g2 then grau ((c,g):t)
                         else grau ((c2,g2):t)

--c
selgrau :: Int -> Polinomio -> Polinomio
selgrau _ [] = []
selgrau x ((c,g):t) = if x == g then (c,g) : selgrau x t
                      else selgrau x t

--d
deriv :: Polinomio -> Polinomio 
deriv [] = []
deriv [(c,0)] = [] 
deriv ((c,g):t) = (c*(fromIntegral g),(g-1)) : deriv t

--e
calcula :: Float -> Polinomio -> Float
calcula _ [] = 0.0
calcula x ((c,g):t) = (c*(x^g)) + calcula x t

--f
simp :: Polinomio -> Polinomio
simp [] = []
simp [(c,0)] = []
simp (x:t) = x : simp t

--g
mult :: Monomio -> Polinomio -> Polinomio
mult _ [] = []
mult (c1,g1) ((c,g):t) = (c1*c,g1+g) : mult (c1,g1) t

--h REPETIR ESTE EXERCÍCIO COM URGÊNCIA
normaliza' :: Polinomio -> Polinomio
normaliza' [] = []
normaliza' [(b,e)] = [(b,e)]
normaliza' ((b, e) : (b2, e2) : ps)   | e == e2 = normaliza' ((b + b2, e) : ps)
                                      | conta e ps == 0 = (b, e) : normaliza' ((b2, e2) : ps)
                                      | otherwise = normaliza' ((b, e) : ps ++ [(b2, e2)])
--i
soma :: Polinomio -> Polinomio -> Polinomio
soma p1 p2 = normaliza' (p1 ++ p2)

--j
produto :: Polinomio -> Polinomio -> Polinomio
produto [] _ = []
produto (h:t) p2 = soma (mult h p2) (produto t p2) 

--k 
ordena :: Polinomio -> Polinomio 
ordena [] = []
ordena (h:t) = insereMon h ( ordena t)


insereMon :: Monomio -> Polinomio -> Polinomio
insereMon x [] = [x]
insereMon (c,g) ((c1,g1):t) = if g >= g1 then (c,g) : ((c1,g1):t)
                              else (c1,g1) : insereMon (c,g) t

--l 
equiv :: Polinomio -> Polinomio -> Bool
equiv p1 p2 | (ordena (normaliza' p1) == ordena (normaliza' p2)) = True
            | otherwise =  False





