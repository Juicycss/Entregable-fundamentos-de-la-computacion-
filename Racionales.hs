{-#LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

{- Agustin Bello (360343) Luciano Liori(379030)-}

import Naturales

data Signo where { Pos :: Signo ; Neg :: Signo } deriving Show
data Racional where { Q :: Signo -> (N,N) -> Racional } deriving Show

instance Eq Signo where
 (==) = \m n -> case m of {
    Pos -> case n of { Pos -> True; Neg -> False };
    Neg -> case n of { Pos -> False; Neg -> True } }

instance Eq Racional where
 (==) = \m n -> (m <= n) && (n <= m) -- esto es el 1, hay q remplazarlo con lo tuyo
 

instance Ord Racional where
 (<=) = \m n -> case m of {
    Q s1 (n1 , d1) -> case n of {
        Q s2 (n2 , d2) -> case s1 of {
            Neg -> case s2 of {
                Pos -> True;
                Neg -> (n2 * d1) <= (n1 * d2) };
            Pos -> case s2 of {
                Pos -> (n1 * d2) <= (n2 * d1);
                Neg -> False } 
                } 
                } 
                }


instance Num Racional where
 (+) = \m n -> case m of {
    Q s1 (n1 , d1) -> case n of {
        Q s2 (n2 , d2) -> case s1 of {
            Neg -> case s2 of {
                Neg -> Q Neg ((n1 * d2) + (n2 * d1)); 
                Pos -> --no se
                 };
            Pos -> case s2 of {
                Neg -> -- ??
                Pos -> Q Pos ((n1 * d2) + (n2 * d1) ) 
                }
                 }
                  }
                   }

 (*) = \m n -> case m of {
    Q s1 (n1 , d1) -> case n of {
        Q s2 (n2 , d2) -> case s1 of {
            Neg -> case s2 of {
                Neg -> Q Pos (n1 * n2 , d1 * d2);
                Pos -> Q Neg (n1 * n2 , d1 * d2) };
            Pos -> case s2 of {
                Neg -> Q Neg (n1 * n2 , d1 * d2);
                Pos -> Q Pos (n1 * n2 , d1 * d2) } 
                }
                 }
                  }

 (-) = \m n -> case m of {
    Q s1 (n1 , d1) -> case n of {
        Q s2 (n2 , d2) -> case s1 of {
            Neg -> case s2 of {
                Neg -> -- lo miso aca
                Pos -> Q Neg ((n1 * d2) - (n2 * d1) , d1 * d2) };
            Pos -> case s2 of {
                Neg -> Q Pos ((n1 * d2) - (n2 * d1) , d1 * d2);
                Pos -> ; -- no se como terminarla
                 }
                 } 
                } 
                }
                