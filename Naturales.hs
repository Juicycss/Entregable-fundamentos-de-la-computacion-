{- Agustin Bello (360343) Luciano Liori(379030)-}
{-#LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
module Naturales where


data N where { O :: N ; S :: N -> N } deriving Show

uno :: N
uno = S O
dos :: N
dos = S uno
tres :: N
tres = S dos
cuatro :: N
cuatro = S tres
cinco :: N
cinco = S cuatro

predecesor :: N -> N
predecesor = \n -> case n of {
    O -> O; 
    S x -> x}


instance Eq N where
 (==) = \n n1 -> case n of {
    O -> case n1 of {
        O -> True;
        S y -> False};
    S x -> case n1 of {
        O -> False;
        S y -> x == y }}

instance Ord N where
 (<=) = \n n1 -> case n of{
    O -> True;
    S x -> case n1 of{
        O -> False;
        S y -> x <= y } }


minimo :: N -> N -> N
minimo = \n n1 -> case n of{
    O -> n;
    S x -> case n1 of{
        O -> n1;
        S y -> S (minimo x y) } }

maximo :: N -> N -> N
maximo = \n n1 -> case n of{
    O -> n1;
    S x -> case n1 of{
        O -> n;
        S y -> S (maximo x y) } }


min3 :: N -> N -> N -> N
min3 = \n n1 n2 -> case n of{
    O -> n;
    S x -> case n1 of{
        O -> n1;
        S y -> case n2 of{
            O -> n2;
            S z -> S(min3 x y z) } } }

min32 :: N -> N -> N -> N
min32 = \n n1 n2 -> minimo (minimo n n1) n2;

instance Num N where
 (+) = \n n1 -> case n of{
    O -> n1;
    S x -> S (x + n1) }
 (*) = \n n1 -> case n of{
    O -> O;
    S x -> case n1 of{
        O -> O;
        S y -> n1 + (x * n1)} }
 (-) = \n n1 -> case n of{
    O -> O;
    S x -> case n1 of {
        O -> n;
        S y -> (x - y)} }

(%):: N -> N -> N
(%) = \n n1 -> case n of{
    O -> O;
    S x -> case n1 of{
        O -> S (O);
        S y -> n * (n % y) } }

doble:: N -> N
doble = \n -> n * dos;

fact:: N -> N
fact = \n -> case n of{
    O -> S (O);
    S x -> n * (fact x) }

sumi :: N -> N
sumi = \n -> case n of{
    O -> O;
    S x -> n +(sumi x)}

sumidobles:: N -> N
sumidobles = \n -> dos * (sumi n);

sumafacts:: N -> N
sumafacts = \n -> case n of{
    O -> O;
    S x -> (fact n) + (sumafacts x)
}