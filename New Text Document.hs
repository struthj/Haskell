module Value where

data Value = MyInt Int | MyBool Bool
    deriving (Eq,Show)

toInt:: Value -> Int
toInt (MyInt x) = x
