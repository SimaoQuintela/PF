import Data.List 
--1
data ExpInt = Const Int 
            | Simetrico ExpInt
            | Mais ExpInt ExpInt
            | Menos ExpInt ExpInt
            | Mult ExpInt ExpInt

--a
calcula :: ExpInt -> Int 
calcula x = case x of
    Const x -> x
    Simetrico x -> - calcula x
    Mais x y -> calcula x + calcula y
    Menos x y -> calcula x - calcula y
    Mult x y -> calcula x * calcula y

--b
infixa :: ExpInt -> String 
infixa x = case x of
    Const x -> show x  
    Simetrico x -> '-' : '(' : infixa x ++ ")"
    Mais x y -> '(' : infixa x ++ "+" ++ infixa y ++ ")"
    Menos x y -> '(' : infixa x ++ "-" ++ infixa y ++ ")"
    Mult x y -> '(' : infixa x ++ "*" ++ infixa y ++ ")"

--c
posfixa :: ExpInt -> String 
posfixa x = case x of
    Const x -> show x
    Simetrico x -> posfixa x ++ "-"
    Mais x y ->  posfixa x ++ " " ++ posfixa y ++ " +"
    Menos x y -> posfixa x ++ " " ++ posfixa y ++ " -"
    Mult x y -> posfixa x ++ " " ++ posfixa y ++ " *"

data RTree a = R a [RTree a]
             deriving Show
--a
soma :: Num a => RTree a -> a
soma (R a l) = a + sum (map soma l)

--b
altura :: RTree a -> Int 
altura (R a []) = 1
altura (R a l) = 1 + maximum (map altura l)

--c 

prune :: Int -> RTree a -> RTree a 
prune 1 (R a l) = R a []
prune x (R a [] ) | x > 1 = R a []
prune x (R a l) = R a (map(prune (x-1)) l)

--d
mirror :: RTree a -> RTree a
mirror (R a []) = R a []
mirror (R a l) = R a (map mirror (reverse l))

--e repetir isto
postorder :: RTree a -> [a]
postorder (R a l) = concatMap postorder l ++ [a]

--3
data LTree a = Tip a | Fork (LTree a) (LTree a)
             deriving Show
--a
ltSum :: Num a => LTree a -> a 
ltSum (Tip a) = a 
ltSum (Fork e d) = ltSum e + ltSum d 

--b
listaLT :: LTree a -> [a]
listaLT (Tip a) = [a] 
listaLT (Fork e d) = listaLT e ++ listaLT d 

--c
ltHeight :: LTree a -> Int 
ltHeight (Tip a) = 1
ltHeight (Fork e d) = 1 + max (ltHeight e) (ltHeight d)

--4
data FTree a b = Leaf b | No a (FTree a b) (FTree a b)

data BTree a = Empty | Node a (BTree a) (BTree a)
--a
splitFTree :: FTree a b -> (BTree a, LTree b)
splitFTree (Leaf b) = (Empty, Tip b)
splitFTree (No r e d) = (Node r x z , Fork y w)
    where (x,y) = splitFTree e 
          (z,w) = splitFTree d 

--b
joinTrees :: BTree a -> LTree b -> Maybe (FTree a b) 
joinTrees Empty (Tip a) = Just (Leaf a) 
joinTrees (Node r e d) (Fork a b) = Just (No r help1 help2)
    where Just help1 = joinTrees e a 
          Just help2 = joinTrees d b
joinTrees _ _ = Nothing 
