data Aposta = Ap [Int] (Int,Int)

{-
geraChave :: IO Aposta 
geraChave = do         x <- generateFiveNums
                       y <- generateFiveStars
                       let k = Ap x y  
                       if valida k then return k 
                       else geraChave



generateFiveNums :: IO [Int]
generateFiveNums = do  n1 <- randomRIO (1,50)
                       n2 <- randomRIO (1,50)
                       n3 <- randomRIO (1,50)
                       n4 <- randomRIO (1,50)
                       n5 <- randomRIO (1,50)
                       return [n1,n2,n3,n4,n5]

generateFiveStars :: IO (Int,Int)
generateFiveStars = do  s1 <- randomRIO (1,9)
                        s2 <- randomRIO (1,9)
                        return (s1,s2)

-}

--group
groupp :: Eq a => [a] -> [[a]]
groupp [] = []
groupp (x:xs) = iguaisPrimeiros x (x:xs) : groupp (drop (length (iguaisPrimeiros x (x:xs))) (x:xs))
    where iguaisPrimeiros n [] = []
          iguaisPrimeiros n (h:t) = if n == h then [h] ++ iguaisPrimeiros n t 
                                   else [] 

-- groupp [1,2,2,3,4,4]
