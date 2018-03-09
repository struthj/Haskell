module KarelSemantics where
-- Struth Joseph
import Prelude hiding (Either(..))
import Data.Function (fix)

import KarelSyntax
import KarelState
import KarelExamples

-- -- | Cardinal directions.
-- data Card = North | South | East  | West  deriving (Eq,Show)

-- -- | Directions relative to the current facing.
-- data Dir  = Front | Back  | Right | Left  deriving (Eq,Show)

-- -- | Environment queries.
-- data Test = Not    Test   -- boolean negation
--           | Facing Card   -- am I facing the given cardinal direction?
--           | Clear  Dir    -- can I move in the given relative direction?
--           | Beeper        -- is there a beeper here?
--           | Empty         -- is my beeper bag empty?
--   deriving (Eq,Show)

-- -- | Statements.
-- data Stmt = Shutdown                 -- end the program
--           | Move                     -- move forward
--           | PickBeeper               -- take a beeper
--           | PutBeeper                -- leave a beeper
--           | Turn    Dir              -- rotate in place
--           | Call    Macro            -- invoke a macro
--           | Iterate Int  Stmt        -- fixed repetition loop
--           | If      Test Stmt Stmt   -- conditional branch
--           | While   Test Stmt        -- conditional loop
--           | Block   [Stmt]           -- statement block
-- type World = Pos -> Maybe Int
-- type Robot = (Pos,Card,Int)
-- demoBot = ((1,1),East,1)

test1 :: Card -> Pos -> Pos
test1 c p = neighbor c p
-- | Valuation function for Test.
test :: Test -> World -> Robot -> Bool
test (Not t) (w) ((x,y),c,n) = if (test t (w) ((x,y),c,n)) == True
                                    then False
                                    else True
test (Facing c1) (w) ((x,y),c,n) = if getFacing ((x,y),c,n) == c1
                                    then True
                                    else False
test (Clear d) (w) ((x,y),c,n) = isClear (neighbor (cardTurn d c) (x,y)) w
test (Beeper) (w) ((x,y),c,n) = hasBeeper (x,y) w
test (Empty) (w) ((x,y),c,n) = isEmpty ((x,y),c,n)

-- | Valuation function for Stmt.
stmt :: Stmt -> Defs -> World -> Robot -> Result
stmt Shutdown   _ _ r = Done r
stmt PickBeeper _ w r = let p = getPos r
                        in if hasBeeper p w
                              then OK (decBeeper p w) (incBag r)
                              else Error ("No beeper to pick at: " ++ show p)
stmt PutBeeper d w r = let p = getPos r
                        in if hasBeeper p w
                              then if getBag r > 0
                                    then OK (incBeeper p w) (decBag r)
                                    else Error("No beeper to put.")
                              else if isClear p w
                                    then if getBag r > 0
                                          then OK (incBeeper p w) (decBag r)
                                          else Error("No beeper to put.")
                                    else Error("Blocked at: " ++ show p)
stmt (Turn dir) d w r = OK (w) (setFacing (cardTurn dir (getFacing r)) r)
stmt (Move) d w r = let p = neighbor (getFacing r) (getPos r)
                      in if isClear p w
                        then OK (w) (setPos p r)
                        else Error("Blocked at: " ++ show p)
stmt (Block (x:xs)) d w r = case (stmt x d w r) of
                              (OK (w1) (r1)) -> stmt (Block xs) d w1 r1
                              (Error e) -> Error ( e)
                              (Done r) -> Done (r)
stmt (Block [x]) d w r = stmt x d w r
stmt (Block []) d w r = OK (w) (r)
stmt (If t s1 s2) d w r = if (test t w r)
                              then stmt s1 d w r
                              else stmt s2 d w r
stmt (Call m) d w r = case lookup m d of
                          (Just s) -> stmt s d w r
                          (Nothing) -> Error("Undefined macro: " ++ m) 
stmt (Iterate n s) d w r = if n > 0
                              then case (stmt s d w r) of
                                    (OK (w1) (r1)) ->  stmt (Iterate (n-1) s) d w1 r1
                                    (Error e) -> Error(e)
                                    (Done r) -> Done (r)
                              else (OK (w) (r))

stmt (While t s) d w r = if (test t w r)
                              then case (stmt s d w r) of
                                    (OK (w1) (r1)) -> stmt (While t s) d w1 r1
                                    (Error e) -> Error(e)
                                    (Done r) -> Done (r)
                              else OK (w) (r)

    
-- | Run a Karel program.
prog :: Prog -> World -> Robot -> Result
prog (m,s) w r = stmt s m w r
