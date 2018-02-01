module Minilogo where


import Prelude hiding (Num)
import Data.List

--
-- * Syntax of Minilogo
--

-- num	::=	(any natural number)	
-- var	::=	(any variable name)	
-- macro	::=	(any macro name)	
 			
-- prog	::=	ε   |   cmd ; prog	sequence of commands
 			
-- mode	::=	down   |   up	pen status
 			
-- expr	::=	var	variable reference
-- |	num	literal number
-- |	expr + expr	addition expression
 			
-- cmd	::=	pen mode	change pen mode
-- |	move ( expr , expr )	move pen to a new position
-- |	define macro ( var* ) { prog }  	define a macro
-- |	call macro ( expr* )	invoke a macro

-- 1. Encode the above grammar as a set of Haskell data types

type Num = Int
type Var = String

data Mode = Up
          | Down
  deriving(Eq,Show)

data Expr = V Var
          | N Num
          | Add Expr Expr
  deriving(Eq,Show)


type Macro = String

type Prog = [Cmd]

data Cmd = Pen Mode
         | Move Expr Expr
         | Define Macro [Var] Prog
         | Call Macro [Expr]
  deriving (Eq,Show)


-- line (x1,y1,x2,y2)
-- define line (x1,y1,x2,y2) {
--   pen up; move (x1,y1);
--   pen down; move (x2,y2);
-- }
line::Cmd
line = Define "line" ["x1", "y1","x2","y2"] [Pen Up, Move (V "x1")  (V "y1"),Pen Down, Move (V "x2")  (V "y2")]

-- nix(x,y,w,h){
--   call line (x, y, x+y, y+h);
--   call line (x+w, y, x, y+h);
-- }

nix::Cmd
nix = Define "nix" ["x","y","w","h"] [Call "line" [ V "x",V "y",Add (V "x")(V "w"), Add (V "y")(V "h")],Call "line" [ Add (V "x")(V "w"),V "y",V "x", Add (V "y")(V "h")]]


drawStep:: Int-> Int-> Prog
drawStep x y = [Call "line" [ V (show x),V (show y), V (show (x-1)), V (show y)], Call "line" [ V (show (x-1)),V (show y), V (show (x-1)), V (show (y-1))]] 

macToStr:: Macro -> String
macToStr m = (show m)

steps:: Int -> Prog 
steps 0 = [Pen Up]
steps n = drawStep (n) (n) ++ steps (n - 1)

macCounter:: Prog -> [Macro]
macCounter ((Define m v (y:ys)):xs) = [m] ++ macCounter (y:ys)
macCounter (x:xs) = macCounter (xs)
macCounter [] = []


macros:: Prog -> [Macro]
macros (x:xs) = macCounter [(x)] ++ macCounter (xs)

printVars:: [Expr] -> String
printVars (x:xs) = printExp (x) ++ "," ++ printVars (xs)
printVars [x] = printExp(x)
printVars [] = []


printVar:: [Var] -> String
printVar (x:xs) = x ++ "," ++ printVar (xs)
printVar [x] = x
printVar [] = []

printExp:: Expr -> String
printExp (V x) = x
printExp (N x) = show x
printExp (Add x y) = (printExp x) ++ "+" ++ (printExp y)


printMode:: Mode -> String
printMode Up = "up"
printMode Down = "down"

prettyPrint:: Prog -> String
prettyPrint ((Define m v (y:ys)):xs) = "define " ++ m ++ " " ++ "(" ++  printVar(v) ++ ") " ++ "{ \n" ++ prettyPrint(y:ys) ++ "}\n" ++ prettyPrint(xs)
prettyPrint ((Move x y):xs) = "move (" ++ (printExp (x)) ++ "," ++ (printExp (y)) ++ ");\n" ++ prettyPrint(xs)
prettyPrint ((Pen x):xs) = "pen " ++ (printMode (x)) ++ ";\n" ++ prettyPrint(xs)
prettyPrint ((Call m e):xs) = "call " ++ m ++ "(" ++ (printVars (e)) ++ ");\n" ++ prettyPrint(xs)
prettyPrint [] = []

optE::Expr -> Expr
optE (Add (Add (N x) (N y)) (V v)) = Add (N (x+y)) (V v)
optE (Add ( V v) (Add (N x) (N y)) ) = Add (V v) (N (x+y))
optE (Add (N x) (N y)) = N (x+y)
optE (V s) = (V s)
optE (Add (V x) (V y)) = Add (V x) (V y)
optE (N n) = ( N n)
optE (Add (N x) e) = optE(Add (N x) (optE (e)))
optE (Add e  (N x)) = optE(Add (optE (e)) (N x))
optE (Add (V x) e) = Add (V x) (optE (e))
optE (Add e  (V x)) = Add (optE (e)) (V x)
optE (Add e x) = optE (Add (optE (e)) (optE(x)))

optEs:: [Expr] -> [Expr]
optEs (x:xs) = [optE(x)] ++ optEs(xs)
optEx [x] = optE(x)


-- optE (Add e (N x) ) = optE ( Add (optE(e)) (N x) )
-- optE (Add (N x) e) = optE ( Add (N x) (optE(e)) )
-- optE (Add e (V x) ) = Add (optE(e)) (V x)
-- optE (Add (V x) e) = Add (V x) (optE(e))


-- optE (Add v e) = Add v (optE(e))
-- optE (Add (N x) ( N y)) = (N (x+y))


-- optE (Add (N x) (V m)) = Add (N x)  (optE (Add n m))
-- optE (Add e (N x) ) = Add (optE(e)) (N x)



optP :: Prog -> Prog
optP []                = []
optP ((Move a b) : xs) = (Move (optE a) (optE b)) : (optP xs)
optP ((Call a b) : xs) = (Call a (map optE b)) : (optP xs)
optP (x : xs)          = x : (optP xs)