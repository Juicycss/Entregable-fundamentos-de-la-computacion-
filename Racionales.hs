{- Agustin Bello (360343) Luciano Liori(379030)-}
{-#LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Racionales  where
import Naturales

data Signo where { Pos :: Signo ; Neg :: Signo } deriving Show
data Racional where { Q :: Signo -> (N,N) -> Racional } deriving Show

instance Eq Signo where
 (==) = \s s1 -> case s of{
    Pos -> case s1 of{
        Pos -> True;
        Neg -> False };
    Neg -> case s1 of{
        Pos -> False;
        Neg -> True } }

instance Eq Racional where
 (==) = \r r1 -> case r of{
    Q s (n,d) -> case r1 of{
        Q s1 (n1,d1) -> (s == s1) && (n == n1) && (d == d1) } }

instance Ord Signo where
    (<=) = \s s1 -> case s of {
        Pos -> case s1 of {
            Neg -> False;
            Pos -> True};
        Neg -> case s1 of{
            Pos -> True;
            Neg -> True
        }

    }



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
                Neg -> Q Neg ((n1 * d2) + (n2 * d1) , d1 * d2 );
                Pos -> case (n1 * d2) > (n2 * d1)  of {
                    True -> Q Neg ((n1 * d2) - (n2 * d1) , d1 * d2);
                    False -> Q Pos ((n2 * d1) - (n1 * d2) , d1 * d2)};
                 };
            Pos -> case s2 of {
                Neg ->case (n1 * d2) > (n2 * d1)  of {
                    True -> Q Pos ((n1 * d2) - (n2 * d1) , d1 * d2);
                    False -> Q Neg ((n2 * d1) - (n1 * d2) , d1 * d2)};
                Pos -> Q Pos ((n1 * d2) + (n2 * d1) , d1 * d2 )
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
                Neg -> case (n1 * d2) > (n2 * d1)  of {
                    True -> Q Neg ((n1 * d2) - (n2 * d1) , d1 * d2);
                    False -> Q Pos ((n2 * d1) - (n1 * d2) , d1 * d2)};
                Pos -> Q Neg ((n1 * d2) - (n2 * d1) , d1 * d2) };
            Pos -> case s2 of {
                Neg -> Q Pos ((n1 * d2) + (n2 * d1) , d1 * d2);
                Pos -> case (n1 * d2) > (n2 * d1)  of {
                    True -> Q Pos ((n1 * d2) - (n2 * d1) , d1 * d2);
                    False -> Q Neg ((n2 * d1) - (n1 * d2) , d1 * d2)};
                 }
                 }
                }
                }
