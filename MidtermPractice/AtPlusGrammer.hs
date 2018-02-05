module AtPlus where
import Prelude hiding(Char)

type Char = String

data Code = At Code Int
            | Plus Code Code
            | C Char

printP:: Code -> String
printP (At c i) = printP(c) ++ " @ " ++ show i
printP (Plus c1 c2) = printP(c1) ++ " + " ++ printP(c2)
printP (C c) = c


a = printP ( At (C "b") 3)

b = "type error"

c = printP ( Plus (C "a" ) (At (C "b") 3) )

d = printP ( At (Plus (C "a") (C "b")) 3 )

e = printP ( At (At (At (C "a") 3) 4) 5)