{- Agustin Bello (360343) Luciano Liori(379030)-}

data Signo where { Pos :: Signo ; Neg :: Signo } deriving Show 
data Racional where { Q :: Signo -> (N,N) -> Racional } deriving Show

(==) :: Signo -> Signo -> Bool -- esto es igual a el igual solo que cambie el true false por neg pos
(==) = \m n -> case m of{
    Pos -> case n of{
        Pos -> true;
        Neg-> false;
    }

    Neg -> case b2 of{
        Pos -> true;
        Neg -> true;
    }
}



