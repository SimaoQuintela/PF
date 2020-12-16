-- {-# LANGUAGE BlockArguments #-} 
hi :: IO ()
hi = do putStrLn ("Ola como te chamas?")
        x <- getLine 
        putStrLn ("Boa tarde" ++ " "++ x  ++ "\nComo tens passado?")
        y <- getLine 
        putStrLn ("Queres jogar um jogo? Se sim pressiona 1 , se nao pressiona 2")
        z <- getChar
        if z == '1' then putStrLn "Fixeeeee, vamos lá jogar"
        else putStrLn "ohhhh que pena :( fica para a próxima"


