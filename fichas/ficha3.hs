import Ficha1

type Etapa = (Hora2,Hora2)
type Viagem = [Etapa]

--a
bemConstruido :: Etapa -> Bool
bemConstruido (h1,h2) = ((valida2 h1) && (valida2 h2)) && (depois h2 h1)

--b
viagemBemConstruida :: Viagem -> Bool
viagemBemConstruida [] = True
viagemBemConstruida [h] = bemConstruido h
viagemBemConstruida ((h1,h2):(h3,h4):t) = bemConstruido (h1,h2) && bemConstruido (h2,h3) && viagemBemConstruida ((h3,h4):t)

--c
partidaEchegada :: Viagem -> (Hora2,Hora2)
partidaEchegada [(x,y)] = (x,y)
partidaEchegada (h1:t) = (fst h1,snd (last t))

--d
tempoDeViagem :: Viagem -> Hora2
tempoDeViagem [(h1,h2)] = diferencaHora h1 h2
tempoDeViagem ((h1,h2):t) = adiciona (horaParaMin (diferencaHora h1 h2)) (tempoDeViagem t) 

--e
tempoDeEspera :: Viagem -> Hora2
tempoDeEspera l =  diferencaHora (tempoTotalDeViagem l)  (tempoDeViagem l) 

--OU 
tempoDeEspera2 :: Viagem -> Hora2
tempoDeEspera2 ((e1,e2):(e3,e4):t) = adiciona (diferencaHora e2 e3) (horaParaMin (tempoDeEspera2 (e3,e4):t))

--f
tempoTotalDeViagem :: Viagem -> Hora2
tempoTotalDeViagem ((h1,h2):t) = diferencaHora h1 (snd (last t))


--2
type Poligonal = [Ponto]
--a
compLinha :: Poligonal -> Double
compLinha [x] = 0
compLinha (x:y:ys) = dist x y + compLinha (y:ys)

--b
fechada :: Poligonal -> Bool
fechada [p1,p2] = False
fechada [p1,p2,p3] = p1 == p3
fechada (p1:p2:p3:ps) = fechada (p1:p3:ps)

--c
triangula :: Poligonal -> [Figura]
triangula [p1,p2,p3] = [(Triangulo p1 p2 p3)]
triangula (p1:p2:p3:ps) = (Triangulo p1 p2 p3):triangula (p1:p3:ps)

--d
--area :: Poligonal -> Double
--area p = sum(map (\fig -> area fig) (triangula p))


--e
mover :: Poligonal -> Ponto -> Poligonal 
mover [] p = [p]
mover l p = p:l 

--f
-- ????????

--3
--a
data Contacto = Casa Integer
              | Trab Integer
              | Tlm Integer
              | Email String
              deriving Show
type Nome = String
type Agenda = [(Nome,[Contacto])]

--a
acrescEmail :: Nome -> String -> Agenda -> Agenda
acrescEmail x y [] = [(x,[Email y])]
acrescEmail x y l = l ++ [(x,[Email y])]

--b
verEmails :: Nome -> Agenda -> Maybe [String]
verEmails a [] = Nothing
verEmails a ((n,c):xs) = if a == n then Just (c : verEmails a xs)
                         else verEmails a xs


--c
consTelefs :: [Contacto] -> [Integer]
consTelefs [] = []
consTelefs (x:xs) = case x of 
    Casa x -> x:consTelefs xs
    Trab x -> x:consTelefs xs
    Tlm x -> x:consTelefs xs
    _ -> []

--d
casa :: Nome -> Agenda -> Maybe Integer
casa nome [n,(c:cs)] = if nome == n then case c of Casa x -> Just x
                                                             otherwise -> casa nome [(n,cs)]
                        else Nothing
casa nome ((n,c):agenda) = if nome == n then casa nome [(n,c)] else casa nome agenda

--4
type Dia = Int
type Mes = Int 
type Ano = Int
type Nome2 = String

data Data = D Dia Mes Ano
          deriving Show

type TabDN = [(Nome,Data)]

--a
procura :: Nome2 -> TabDN -> Maybe Data
procura nome ((n,d):t) = if nome == n then Just d
                         else procura nome t

--b
idade :: Data -> Nome2 -> TabDN -> Maybe Int
idade _ _ [] = Nothing
idade (D d m a) x ((n,D d1 m1 a1):xs) = case x of 
    n -> if m > m1 || m == m1 && d > d1 then Just (a-a1)
         else idade (D d m a) x xs 

--c
anterior  :: Data -> Data -> Bool
anterior  (D d m a) (D d1 m1 a1)   | (a==a1 && m==m1 && d==d1) = False 
                                   | a > a1 = False
                                   | (a==a1 && m>m1) = False
                                   | (a==a1 && m==m1 && d>d1) = False
                                   | otherwise = True

--d
ordena :: TabDN -> TabDN 
ordena [] = []
ordena ((n,d):t) = inserirData (n,d) (ordena t)
    where inserirData (a,b) [] = [(a,b)]
          inserirData (a,b) ((a1,b1):t) = if anterior b1 b then (a1,b1): inserirData (a,b) t
                                          else (a,b):(a1,b1):t

--e
porIdade :: Data -> TabDN -> [(Nome,Int)]
porIdade _ [] = []
porIdade (D d m a) l = (n,idade) : porIdade (D d m a) ts 
    where ((n,D dx mx ax):ts) = ordena l
          idade = if m > mx || mx == m && d > dx then (a-ax)
                  else ((a-ax)-1)

--5
data Movimento = Credito Float | Debito Float 
               deriving Show 

data Data3 = P Int Int Int
           deriving Show

data Extrato = Ext Float [(Data,String,Movimento)]
             deriving Show

--a
extValor :: Extrato -> Float -> [Movimento]
extValor (Ext a []) _ = []
extValor  (Ext a ((_,_,mov):xs)) x = case mov of 
                                     Credito c -> if c >= x then mov : extValor (Ext a xs) x else extValor (Ext a xs) x
                                     Debito c -> if c >= x then mov : extValor (Ext a xs) x else extValor (Ext a xs) x

--b
filtro :: Extrato -> [String] -> [(Data,Movimento)]
filtro (Ext x []) _ = []
filtro (Ext x ((d,s,m):xs)) l = if elem s l then (d,m) : filtro (Ext x xs) l
                                else filtro (Ext x xs) l

--c

creDeb :: Extrato -> (Float,Float)
creDeb (Ext a ((_,_,(Credito b)):t)) = (b+x,y)
    where (x,y) = creDeb (Ext a (t)) 
creDeb (Ext a ((_,_,(Debito b)):t)) = (x,b+y)
   where (x,y) = creDeb (Ext a (t))
creDeb _ = (0,0)

--d
-- minha resolução, não sei se tá bem

-- saldo :: Extrato -> Float
-- saldo (Ext a((_,_,mov):xs)) = case mov of
--     Credito b -> b + saldo ((Ext a) xs)
--     Debito b -> -b + saldo ((Ext a) xs)
-- saldo _ = 0

saldo :: Extrato -> Float
saldo (Ext a b) = (a-x+y)
    where (x,y) = creDeb (Ext a b)
