module ExprStaticType where

--    Consider the following abstract syntax for a language for building and manipulating non-nested integer lists.
data Expr = N Int            -- integer value
        | Empty            -- empty integer list
        | Sum Expr         -- sum of an integer list
        | Cons Expr Expr   -- prepend an integer to a list
    deriving(Eq,Show)

-- Your task is to implement a static type system for this language. Note that the language does *not* support nested lists. That is, there are only two valid types in our language: integers and lists of integers, anything else is a type error.

data IExp = Eint | Elist | Etyperr deriving(Eq,Show)

typeOf :: Expr -> IExp
typeOf (N n) = Eint
typeOf Empty = Elist
typeOf (Sum e) = case typeOf e of
                Elist -> Eint
                _ -> Etyperr
typeOf (Cons e1 e2) = case ((typeOf (e1)), (typeOf (e2))) of
                        (Eint, Elist) -> Elist
                        (Elist, Eint) -> Elist
                        _ -> Etyperr

data Val = I Int | L [Int] | Err deriving(Eq,Show)

eval :: Expr -> Val
eval (N n) = (I n)
eval (Empty) = (L [])
eval (Sum e) = case (eval e) of
                (L n) -> (I (sum n))
                _ -> Err
eval (Cons e1 e2) = case (eval e1, eval e2) of
                    ((I n), (L l)) -> (L (n:l))
                    ((L l), (I n)) -> (L (n:l))
                    _ -> Err

