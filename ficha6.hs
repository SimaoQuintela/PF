data BTree a = Empty
             | Node a (BTree a) (BTree a)
             deriving (Show)
altura :: BTree a -> Int
altura Empty = 0 
altura (Node r e d) = 1 + (max (altura e) (altura d))

contaNodos :: BTree a -> Int
contaNodos Empty  = 0
contaNodos (Node r e d) = 1 + (contaNodos e) + (contaNodos d)

folhas :: BTree a -> Int
folhas Empty = 0
folhas (Node _ Empty Empty) = 1  
folhas (Node _ e d) = (folhas e) + (folhas d)

prune :: Int -> BTree a -> BTree a
prune 0 (Node a b c) = Node a b c
prune 1 _ = Empty
prune x Empty = Empty
prune x (Node a b c) = Node a (prune (x-1) b) (prune (x-1) c)



a5 = Node 10 (Node 5 (Node 2 Empty Empty)
               (Node 7 (Node 6 Empty Empty)
                       (Node 8 Empty Empty)))
          (Node 18 (Node 12 Empty Empty)
                   (Node 21 (Node 19 Empty Empty)
                      (Node 35 Empty Empty)))


--         10
--       /     \
--    5          18 
--   /  \       /  \
-- 2      7   12    21
--       / \        / \
--      6   8      19 35

--e
path :: [Bool] -> BTree a -> [a]
path [] _ = []
path l (Node r Empty Empty) = []
path (x:xs)(Node r (Node e1 e d) (Node e2 ee dd)) = case x of
                                                    False -> e1 : path xs (Node e1 e d)
                                                    True -> e2 : path xs (Node e2 ee dd)

--f
mirror :: BTree a -> BTree a
mirror Empty = Empty
mirror (Node r e d) = Node r (mirror d) (mirror e)

--g
zipWithBT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c
zipWithBT f Empty Empty = Empty
zipWithBT f Empty (Node rr ee dd) = Empty
zipWithBT f (Node r e d) Empty = Empty
zipWithBT f (Node r e d) (Node rr ee dd) = Node (f r rr) (zipWithBT f e ee) (zipWithBT f d dd)

--h
unzipBT :: BTree (a,b,c) ->  (BTree a, BTree b, BTree c)
unzipBT Empty = (Empty,Empty,Empty)
unzipBT (Node (a,b,c) e d) = ((Node a l1 r1) , (Node b l2 r2) , (Node c l3 r3))
    where (l1,l2,l3) = unzipBT e
          (r1,r2,r3) = unzipBT d

--2
--a
minimo :: Ord a => BTree a -> a 
minimo (Node r Empty Empty) = r
minimo (Node _ e _) = minimo e

--b
semMinimo :: Ord a => BTree a -> BTree a
semMinimo Empty = Empty 
semMinimo (Node r Empty d) = d
semMinimo (Node r e d) = Node r (semMinimo e) d

--c
minSmin :: Ord a => BTree a -> (a,BTree a)
minSmin (Node r Empty d ) = (r,Empty)
minSmin (Node r e d) = (a, Node r b d)
    where (a,b) = minSmin e
                                                                           
 
 --3
type Aluno = (Numero,Nome,Regime,Classificacao)
type Numero = Int
type Nome = String
data Regime = ORD | TE | MEL deriving Show
data Classificacao = Aprov Int
                   | Rep
                   | Faltou
    deriving (Show,Eq)
type Turma = BTree Aluno -- árvore binária de procura (ordenada por número)

--a
inscNum :: Numero -> Turma -> Bool
inscNum _ Empty = False
inscNum x (Node (a,_,_,_) l r) | x == a = True
                               | x > a = inscNum x r
                               | otherwise = inscNum x l
--b
inscNome :: Nome -> Turma -> Bool
inscNome _ Empty = False
inscNome x (Node (_,s,_,_) e d)   | x == s = True
                                  | otherwise = inscNome x e || inscNome x d

--c
trabEst :: Turma -> [(Numero,Nome)]
trabEst Empty = []
trabEst (Node (a,b,c,_) e d) = case c of
                               TE -> [(a,b)] ++ trabEst e ++ trabEst d
                               otherwise ->  trabEst e ++ trabEst d

--d
nota :: Numero -> Turma -> Maybe Classificacao
nota x (Node (a,_,_,d) l r) | (x == a) = Just d
                            | (x < a) = nota x l
                            | (x > a) = nota x r
nota _ _ = Nothing

--e
percFaltas :: Turma -> Float
percFaltas Empty = 0.0
percFaltas (Node (a,b,c,d) l r) = (contaFaltas (Node (a,b,c,d) l r))/((alunos (Node (a,b,c,d) l r))) * 100

contaFaltas :: Turma -> Float                      
contaFaltas Empty = 0.0
contaFaltas (Node (a,b,c,d) l r) | (d == Faltou) = 1 + contaFaltas l + contaFaltas r
                                 | otherwise = contaFaltas l + contaFaltas r
alunos :: Turma -> Float
alunos Empty = 0.0
alunos (Node (a,b,c,d) l r) = 1 + alunos l + alunos r 
--
--f

mediaAprov :: Turma -> Float
mediaAprov Empty = 0
mediaAprov turma = (somaDasNotas turma) / (contaAprovados turma)

contaAprovados :: Turma -> Float
contaAprovados Empty = 0
contaAprovados (Node (_,_,_,n) l r) = case n of
                                      Aprov _ -> 1.0 + contaAprovados l + contaAprovados r
                                      otherwise -> contaAprovados l + contaAprovados r

somaDasNotas :: Turma -> Float
somaDasNotas Empty = 0
somaDasNotas (Node (_,_,_,d) l r) = case d of
                                    Aprov x -> fromIntegral x + somaDasNotas l + somaDasNotas r
                                    otherwise -> somaDasNotas l + somaDasNotas r

--g     Percorre a árvore apenas 1 vez

aprovAV2 :: Turma -> Float
aprovAV2 Empty = 0 
aprovAV2 (Node (a,b,c,d) l r) = case d of 
                                Aprov x -> (1 + aprovAV2 l + aprovAV2 r) / (1 + aprovAV2 l + aprovAV2 r)
                                Rep -> (aprovAV2 l + aprovAV2 r) / (1+ aprovAV2 l + aprovAV2 r)
                                otherwise -> (aprovAV2 l + aprovAV2 r) / (aprovAV2 l + aprovAV2 r)
