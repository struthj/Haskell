module Expr where
import Prelude

data Expr = I Int
            | B Bool
            | Add Expr Expr
            | If Expr Expr Expr
    deriving(Eq)


ex1 = If (B True) (Add (I 3) (I 3)) (I 6)

-- Semantic domain is the set of all ints and Boolean T/F values

data Val =  N Int | Bo Bool | TypeErr

printVal:: Val -> String
printVal (N n) = show n
printVal (Bo b) = show b
printVal TypeErr = "TypeErr"

sem:: Expr -> Val
sem (I n) =  N n
sem (B b) = Bo b
sem (Add e1 e2) = case (sem e1, sem e2) of
                            (N i, N j) -> (N (i + j))
                            _ -> TypeErr
sem (If e1 e2 e3) = case ( sem e1) of
                            (Bo True) -> sem e2
                            (Bo False) -> sem e3
                            _ -> TypeErr    

