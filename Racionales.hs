{- Agustin Bello (360343) Luciano Liori(379030)-}

data Signo where { Pos :: Signo ; Neg :: Signo } deriving Show 
data Racional where { Q :: Signo -> (N,N) -> Racional } deriving Show

(==) :: Signo -> Signo -> Bool -- esto es igual a el igual solo que cambie el true false por neg pos no se si ponerlo asi o como instance Eq singo where
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

instance Ord Racional where
    (<=) = \m n -> case m of{
        Q s1 (n1 , d1) -> case n of{ -- sg num 1
        Q s2 (n2 , d2) -> case s1 of{ -- sg num 2

            Neg -> case s2 of{
                Pos -> True;
                Neg ->(n1 * d2) <= (n2 *d1);
            }
              Pos -> case s2 of{
                Pos ->(n2 * d1) <= (n1 *d2);
                Neg -> False;
            }
        }
        } 
    }
{- tengo a s1(pos) pasa a ver s2(pos). 
vuelve a Qs1 al ser negativo, ve casos en n(S2) si es negativo y qs2/n es Pos
, devuelve true, en caso de que sea negativo, hago denominador comun, y si el numero mas chico
es el de la izquierda, devuelve true.

<= uso esto que ya esta implementado en naturales.
}