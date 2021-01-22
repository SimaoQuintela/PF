

data FTree a b = Leaf b | No a (FTree a b) (FTree a b)

ftree1 = No 8 (No 1 (Leaf 'g')
                    (No 2 (Leaf 'c')
                          (Leaf 'd')))
              (No 9 (No 10 (Leaf 'e')
                           (Leaf 'f'))
                    (Leaf 'p')

data LTree a = Tip a | Fork (LTree a) (LTree a)

data BTree a = Empty | Node a (BTree a) (BTree a)

splitFTree :: FTree a b -> (BTree a, LTree b)
splitFTree (Leaf b) = (Empty , Tip b)
splitFTree (No a l r) = (Node a x y , Fork z w)
    where (x,z) = splitFTree l 
          (y,w) = splitFTree r

joinTrees :: BTree a -> LTree b -> Maybe (FTree a b)
joinTrees (Node r Empty Empty) (Tip b) = Just (Leaf b)
joinTrees (Node s e d) (Fork l r) = Just (No s x y) 
    where x = joinTrees e l 
          y = joinTrees d r
joinTrees _ _ = Nothing 