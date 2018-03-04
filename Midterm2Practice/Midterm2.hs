module Midterm2 where

type Hour = Int
type Minutes = Int
    
data Time = Midnight
            | Noon
            | AM Hour
            | PM Hour
            | Before Minutes Time  
            | After Minutes Time
        deriving(Eq,Show)


-- (a) Implement a denotational semantics for this language using Int as the semantic domain, where the integer represents the number of minutes since midnight. For example, the time 8:13am could be represented by the expression After 13 (AM 8), and would be mapped to the semantic value 493. For this version of the semantics, you may assume that all hour values are between 1 and 12. It is OK for the resulting semantic value to be negative or a number larger than the number of minutes in a 24-hour day.

semT :: Time -> Int
semT Midnight = 0
semT Noon = 720
semT (AM h) = (60 * h)
semT (PM h) = (720 + (60*h))
semT (Before m t) = case (semT t) of
                    t1 -> (t1 - m)
semT (After m t) = case (semT t) of
                    t1 -> (m + t1)


-- (b) Implement a revised version of this denotational semantics that checks to make sure that all hour values are between 1 and 12, and returns an error otherwise.

data Tres = OK Int
            | TypeErr
        deriving(Eq,Show)

semR :: Time -> Tres
semR Midnight = OK 0
semR Noon = OK 720
semR (AM h) = if h > 0 && h <= 12
                then OK (semT (AM h))
                else TypeErr
semR (PM h) = if h > 0 && h <= 12
                then OK (semT (PM h))
                else TypeErr
semR (Before m t) = case (semR t) of 
                    OK i -> OK (m - i)
                    _ -> TypeErr
semR (After m t) = case (semR t) of 
                    OK i -> OK (m + i)
                    _  -> TypeErr

                    
            