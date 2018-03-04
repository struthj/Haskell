module Position where

type Pos = (Int,Int)

data Move = JumpTo Pos     -- immediately move to the given position
            | GoUp Int       -- move vertically
            | GoRight Int    -- move horizontally
            | Seq Move Move  -- do one move followed by another

-- The JumpTo construct immediately moves to the given position. The GoUp construct moves the current position vertically the indicated number of steps (a negative value will move the current position down). The GoRight construct moves the current position the indicated number of steps horizontally (negative = left). The Seq construct performs the left move followed by the right move.

-- Define a denotational semantics for this language.

-- Take in Move command and Starting position, returns modified Pos

-- Define the abstract syntax, T data Term = ...
-- the set of abstract syntax trees
-- 2. Identify or define the semantics domain, V type Value = ...
-- the representation of semantic values
-- 3. Define the valuation function, J·K : T → V sem :: Term -> Value
-- the mapping from ASTs to semantic values

semM :: Move -> Pos -> Pos
semM (JumpTo (x,y)) (x1,y1) = (x,y)
semM (GoUp n) (x1,y1) = (x1,(y1 + n))
semM (GoRight n) (x,y) = (x + n, y)
semM (Seq m1 m2) (x, y) = case (semM m1 (x,y)) of
                            (x1,y1) -> semM m2 (x1,y1)

