import Data.List
import Data.Char
-------------
{- OFF AULA
data Date = D Int Int Int
          deriving Eq
instance Eq Date where
(D d1 m1 a1 == D d2 m2 a2) = (m1 == m2)

----------------

instance show Date where
   show D d m a = "dia" ++ show d ++ "do" ++ show m ++ "do"

-}
data Frac = F Integer Integer
--1
--a
normaliza :: Frac -> Frac
normaliza (F x y) =  F (x `div` (mdc x y)) (y `div` (mdc x y))
--- ou
--forma da stora
normaliza' :: Frac -> Frac
normaliza' (F x y) = F (div x m) (div y m)
    where m = mdc x y


mdc :: Integer -> Integer -> Integer
mdc x 0 = x
mdc x y = mdc y (mod x y)



--b  pode correr mal se a multiplicacao for muito grande
instance Eq Frac where
    (F x y) == (F x1 y1) = (x * y1) == ( x1 * y)
                    -- ou normaliza (F x y) == normaliza (F x1 y1)

--c Ir ver o data.Ord , data.Eq .........
{-
ord é definido por LT <
                   EQ =
                   GT > -}

instance Ord Frac where
  compare (F x1 y1) (F x2 y2)
    |c1 < c2 = LT
    |c1 == c2 = EQ -- ou | (F x1 y1) == (F x2 y2) = EQ
    |c1 > c2 = GT
    where
      c1 = (fromIntegral x1) / (fromIntegral y1)
      c2 =  (fromIntegral x2) / (fromIntegral y2)
    --  f1 < f2 -> LT
    -- f1 == f2 -> Eq -- |
    -- f1 > f2 -> GT

{-
--d
instance Show Frac where
  show (F x y) = show x ++ "/" ++ show y

--e
instance Num Frac where
 (+) = normaliza' (somaFrac)
 (*) = normaliza' (multiplica)
 (-) = subtrai'
 negate (F x y) = (F (-x) y)
 abs (F x y) = (F (abs x) (abs y))
 signum (F x y) = (F ((signum x) * (signum y)) (1))
   --normaliza (F (signum x) (signum y))
 fromInteger x = F x 1

somaFrac' :: Frac -> Frac -> Frac
somaFrac' (F x1 y1) (F x2 y2) = normaliza ((F (x1*y2 + x2*y1) (y1*y2)))

multiplica' :: Frac -> Frac -> Frac
multiplica' (F x y) (F x1 y1) = (F (x*x1) (y*y1))

subtrai' :: Frac -> Frac -> Frac
subtrai' (F x y) (F x1 y1) = normaliza ((F (x1*y2 - x2*y1) (y1*y2)))


--f
maiores :: Frac -> [Frac] -> [Frac]
maiores f l = filter (\x -> f*(fromInteger2) < x) l

--2
data Exp a = Const a
           | Simetrico (Exp a)
           | Mais (Exp a) (Exp a)
           | Menos (Exp a) (Exp a)
           | Mult (Exp a) (Exp a)

--fun :: Num a => a -> a

--a
instance Num a => Num (Exp a) where
  (+) e1 e2 = Mais e1 e2
  (*) e1 e2 = Mult e1 e2
  (-) e1 e2 = Menos e1 e2
  signum e = Const  (signum (calcula e))
  abs e = e * signum e
  fromInteger n = Const (fromInteger n)   --- o fromInteger da esquerda é diferente do da esquerda

  -- (+) , (*) , (-) :: a -> a -> a
  --          negate, abs, signum :: a -> a
  --          fromInteger :: Integer -> a
-}

--3
data Movimento = Credito Float | Debito Float
data Data = D Int Int Int deriving Eq 
data Extracto = Ext Float [(Data,String,Movimento)]

--a
instance Ord Data where 
    compare (D d m a) (D d1 m1 a1) | (m<m1 && a==a1) || a<a1 || (d<d1 && m==m1 && a==a1) = LT
                                   | d==d1 && m==m1 && a==a1 = EQ 
                                   | otherwise = GT

--b
instance Show Data where
    show (D d m a) = "dia" ++ show d ++ "de" ++ show m ++ "de" ++ show   (será que posso fazer assim?)
--------------------------------- OU  ---------------------------------------------
    show (D d m a) = concat $ intersperse "/" $ map (show) [d,m,a]

--c repetir isto 
ordena :: Extracto -> Extracto 
ordena (Ext n l) = (Ext n (sortBy (\(data1,_,_) (data2,_,_) -> compare data1 data2) l ))

--d repetir isto
instance Show Extracto where 
    show (Ext n l) = "Saldo anterior:" ++ show n ++
                     "\n-------------------------------------------------" ++
                     "\nData           Descricao        Credito    Debito" ++
                     "\n-------------------------------------------------\n" ++ concatMap (\(dat,str,mov) -> show dat ++ replicate (11- (length (show dat ))) ' ' ++ map (toUpper) str ++ "    \n") l ++
                     "--------------------------------------------------" ++
                     "\nSaldo actual:" ++ show (saldo (Ext n l))

saldo :: Extracto -> Float
saldo (Ext a l) = foldl (\acc (_,_,mov) -> case mov of Credito n -> (acc +n)
                                                       Debito n -> (acc -n)) a l
