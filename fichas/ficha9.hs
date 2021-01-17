import System.Random

--1
--a  Jogo do Bingo 
bingo :: IO ()
bingo = do x <- acumula []
           print x

acumula ::[Int] -> IO [Int]
acumula l | length l == 90 = return l 
          | otherwise = do y <- randomRIO (1,90)
                           print y
                           getChar
                           let z = if elem y l then l else y : l in acumula l 

--b


--2
data Aposta = Ap [Int] (Int,Int)
            deriving Show 
--a depois tentar simplificar 
valida :: Aposta -> Bool
valida (Ap l (e1,e2)) | length l == 5 && all (\x -> x>0 && x<= 50) l && verifica l && elem e1 [1..9] && elem e2 [1..9] && (e1 /= e2) = True
                      | otherwise = False 

verifica :: [Int] -> Bool
verifica [x] =  True
verifica (x:y:xs) = if elem x (y:xs) then False  
                    else verifica (y:xs) 

--b  depois tentar simplificar 
comuns :: Aposta -> Aposta -> (Int,Int)
comuns (Ap l1 e1) (Ap l2 e2) = (numeros l1 l2 ,estrelas e1 e2)

numeros :: [Int] -> [Int] -> Int 
numeros [] _ = 0 
numeros (h:t) l = if elem h l then 1 + numeros t l 
                  else numeros t l

estrelas :: (Int,Int) -> (Int, Int) -> Int 
estrelas (e1,e2) (a,b) | e1 == a && e2 == b || e1 == b && e2 == a = 2
                       | e1 == a || e1 == b || e2 == a || e2 == b = 1
                       | otherwise = 0  

--c
--i
instance Eq Aposta where 
    a == b = comuns a b == (5,2) 

--ii  depois tentar simplificar
premio :: Aposta -> Aposta -> Maybe Int 
premio x y | comuns x y == (5,2) = Just 1 
           | comuns x y == (5,1) = Just 2 
           | comuns x y == (5,0) = Just 3 
           | comuns x y == (4,2) = Just 4
           | comuns x y == (4,1) = Just 5
           | comuns x y == (4,0) = Just 6
           | comuns x y == (3,2) = Just 7
           | comuns x y == (2,2) = Just 8
           | comuns x y == (3,1) = Just 9
           | comuns x y == (3,0) = Just 10
           | comuns x y == (1,2) = Just 11 
           | comuns x y == (2,1) = Just 12 
           | comuns x y == (2,0) = Just 13
           | otherwise = Nothing 

--e 
geraChave :: IO Aposta
geraChave = do  x <- generateFiveNums 
                y <- generateTwoStars
                let k = (Ap x y)
                if valida k then return k else geraChave  


generateFiveNums :: IO [Int]
generateFiveNums = do n1 <- randomRIO (1,50)
                      n2 <- randomRIO (1,50)
                      n3 <- randomRIO (1,50)
                      n4 <- randomRIO (1,50)
                      n5 <- randomRIO (1,50)
                      return [n1,n2,n3,n4,n5]

generateTwoStars :: IO (Int,Int)
generateTwoStars = do s1 <- randomRIO (1,9)
                      s2 <- randomRIO (1,9)
                      return (s1,s2)

--f 
{-
main :: IO ()
main = do ch <- geraChave
          ciclo ch 

ciclo :: Aposta -> IO () 
ciclo key = do x <- menu
               case menu of "1" -> do joga key ; ciclo key 
                            "2" -> do main 
                            "0" -> putStrLn "Fim do jogo"

menu :: IO String 
menu = do { putStrLn menutxt
          ; putStr "Opcao: "
          ; c <- getLine 
          ; return c 
          } 
      where menutxt = unlines [" ",
                               "Apostar ............. 1",
                               "Gerar nova chave .... 2",
                               " ",
                               "Sair ................ 0"]


         -}