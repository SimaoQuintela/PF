import Data.List
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
deleteBy :: (a -> a -> Bool) -> a -> [a] -> [a]
deleteBy _ _ [] = []
deleteBy f x (h:t) | f x h = t
                   | otherwise = h : deleteBy f x t

--g
sortOn :: Ord b => (a->b) -> [a] -> [a]
sortOn f [] = []
sortOn f (x:xs) = inserir x (sortOn f xs)
    where inserir x [] = [x]
          inserir x (h:t) = if f x > f h then h : inserir x t
                            else x:h:t

--2
type Polinomio = [Monomio]
type Monomio = (Float,Int)

--a
selgrau :: Int -> Polinomio -> Polinomio
selgrau
