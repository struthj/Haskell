module Nat where
import Data.List (foldr)
import Prelude hiding (Enum(..),sum)
--
-- * Part 2: Natural numbers
--

-- | The natural numbers.
data Nat = Zero
         | Succ Nat
         deriving (Eq,Show)

-- | The number 1.
one :: Nat
one = Succ Zero

-- | The number 2.
two :: Nat
two = Succ one

-- | The number 3.
three :: Nat
three = Succ two

-- | The number 4.
four :: Nat
four = Succ three


-- | The predecessor of a natural number.
--   
--   >>> pred Zero
--   Zero
--   
--   >>> pred three
--   Succ (Succ Zero)
--   
pred:: Nat -> Nat
pred Zero = Zero
pred (Succ n) = n


-- | True if the given value is zero.
--
--   >>> isZero Zero
--   True
--
--   >>> isZero two
--   False
--
isZero:: Nat -> Bool
isZero Zero = True
isZero _ = False


-- | Convert a natural number to an integer.
--
--   >>> toInt Zero
--   0
--
--   >>> toInt three
--   3
--
toInt:: Nat -> Int
toInt (Zero) = 0
toInt n = 1 + (toInt (pred n))


-- | Add two natural numbers.
--
--   >>> add one two
--   Succ (Succ (Succ Zero))
--
--   >>> add Zero one == one
--   True
--
--   >>> add two two == four
--   True
--
--   >>> add two three == add three two
--   True
--   
add:: Nat -> Nat -> Nat
add (Zero) n = n
add (Succ n) m = add n (Succ m)


-- | Subtract the second natural number from the first. Return zero
--   if the second number is bigger.
--
--   >>> sub two one
--   Succ Zero
--   
--   >>> sub three one
--   Succ (Succ Zero)
--
--   >>> sub one one
--   Zero
--
--   >>> sub one three
--   Zero
--
sub:: Nat -> Nat -> Nat
sub n Zero = n
sub (Succ n) (Succ m) = sub n m
sub n m
    | (toInt n) > (toInt m) = Zero
    | otherwise = (n)

-- | Is the left value greater than the right?
--
--   >>> gt one two
--   False
--
--   >>> gt two one
--   True
--
--   >>> gt two two
--   False
--
gt:: Nat -> Nat -> Bool
gt n m = 
    if (toInt n) > (toInt m)
        then True
    else False


-- | Multiply two natural numbers.
--
--   >>> mult two Zero
--   Zero
--
--   >>> mult Zero three
--   Zero
--
--   >>> toInt (mult two three)
--   6
--
--   >>> toInt (mult three three)
--   9
--
mult:: Nat -> Nat -> Nat
mult _ Zero = Zero
mult Zero _ = Zero
mult (Succ n) m = add (mult n m) m


-- | Compute the sum of a list of natural numbers.
--
--   >>> sum []
--   Zero
--   
--   >>> sum [one,Zero,two]
--   Succ (Succ (Succ Zero))
--
--   >>> toInt (sum [one,two,three])
--   6
--
toNat::Int -> Nat
toNat 0 = Zero
toNat n = Succ (toNat (n -1))

sum:: [Nat] -> Nat
sum [] = Zero
sum list = toNat (foldr (+) 0 (map toInt list))


-- | An infinite list of all of the *odd* natural numbers, in order.
--
--   >>> map toInt (take 5 odds)
--   [1,3,5,7,9]
--
--   >>> toInt (sum (take 100 odds))
--   10000
--
odds = undefined
