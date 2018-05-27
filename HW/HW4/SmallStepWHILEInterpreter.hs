---------------------
-- Program: SmallStepsWHILEinterpreter.hs
-- Authors: Johnnie Chang and Wei-Lin Wu
-- On this homework, we worked together for 8 hours;
-- Johnnie worked independently for 4 hours,
-- and Wei-Lin worked independently for 5 hours.
-- This program modifies the one for HW 2.
-- Based on the feedback from TA on HW 2, we reimplementated states using hash map from string to int,
-- rather than a function from variable to int. This is much more efficient than our earlier implementation.
-- We also changed the representation of variable as indexed by int to one as indexed/recognized by string.
-- When displayed on the screen, the variables are surrounded by double quotes (so that "1" is a variable).
-- Finally, we fixed the semantics of WhileDo, RepeatUntil, and ForFromToDo based on their respective
-- recursive structure with IfThenElse (so that each of their semantics is expressed through itself and
-- the semantics of IfThenElse).
-- You're welcome to use both evalComm and evalCommSeq to check the correctness of our program.
--
-- Please use putStrLn when testing in terminal as this correctly displays the newline characters ('\n'), i.e.
-- displaying a new line instead of literally backslash n.
----------------------

import Data.HashMap



--Id synonymous to String
type Id = String



--Variable constructed from Id, i.e. from String
data Variable = V Id deriving Eq
instance Show Variable where
	show (V str) = "\"" ++ str ++ "\""



--State is a map from Id to Int
type State = HashMap Id Int

sampleState :: State
sampleState = fromList [("x", 2), ("y", 3), ("z", 5)]

--this is to be used as an auxiliary function for show'
display :: [(Id, Int)] -> String
display [] = ""
display ((v, n) : vs) = if vs == []
	                        then show v ++ " = " ++ show n
	                        else show v ++ " = " ++ show n ++ ", " ++ display vs

--print State on the screen
--show has already been defined for State, so we chose to name it show'
show' :: State -> String
show' s = if Data.HashMap.null s then "" else display strList where strList = toList s



--AST of Aexp
data Aexp = Num Int | Var Id | Add Aexp Aexp | Sub Aexp Aexp | Mul Aexp Aexp | Div Aexp Aexp
            | Exp Aexp Aexp | Fac Aexp | Bin Aexp Aexp
			
instance Show Aexp where
	show (Num n)     = show n
	show (Var i)     = show (V i)
	show (Add a1 a2) = "(" ++ show a1 ++ ") + (" ++ show a2 ++ ")"
	show (Sub a1 a2) = "(" ++ show a1 ++ ") - (" ++ show a2 ++ ")"
	show (Mul a1 a2) = "(" ++ show a1 ++ ") * (" ++ show a2 ++ ")"
	show (Div a1 a2) = "(" ++ show a1 ++ ") / (" ++ show a2 ++ ")"
	show (Exp a1 a2) = "(" ++ show a1 ++ ") ^ (" ++ show a2 ++ ")"
	show (Fac a)     = "(" ++ show a ++ ")!"
	show (Bin a1 a2) = "C(" ++ show a1 ++ ", " ++ show a2 ++ ")"
	
--evaluation of Aexp
evalAexp :: Aexp -> State -> Int
evalAexp (Num n)     s = n
evalAexp (Var i)     s = s ! i
evalAexp (Add a1 a2) s = (evalAexp a1 s) + (evalAexp a2 s)
evalAexp (Mul a1 a2) s = (evalAexp a1 s) * (evalAexp a2 s)
evalAexp (Sub a1 a2) s = (evalAexp a1 s) - (evalAexp a2 s)
evalAexp (Div a1 a2) s = (evalAexp a1 s) `dvs` (evalAexp a2 s)
evalAexp (Exp a1 a2) s = pow (evalAexp a1 s) (evalAexp a2 s)
evalAexp (Fac a)     s = fac (evalAexp a s) 
evalAexp (Bin a1 a2) s = bin (evalAexp a1 s) (evalAexp a2 s)



--AST of Bexp
data Bexp = T | F | Equal Aexp Aexp | LessThan Aexp Aexp | Not Bexp | And Bexp Bexp | Or Bexp Bexp

instance Show Bexp where
	show  T               = "true"
	show  F               = "false"
	show (Equal a1 a2)    = show a1 ++ " == " ++ show a2
	show (LessThan a1 a2) = show a1 ++ " < " ++ show a2
	show (Not b)          = "!(" ++ show b ++ ")"
	show (And b1 b2)      = (show b1) ++ " && " ++ (show b2)
	show (Or b1 b2)       = (show b1) ++ " || " ++ (show b2)

--evaluation of Bexp (And, Or shortcircuited)
evalBexp :: Bexp -> State -> Bool
evalBexp  T               s = True
evalBexp  F               s = False
evalBexp (Equal    a1 a2) s = (evalAexp a1 s) == (evalAexp a2 s)
evalBexp (LessThan a1 a2) s = (evalAexp a1 s) < (evalAexp a2 s)
evalBexp (Not      b)     s = if evalBexp b s then False else True
evalBexp (And      b1 b2) s = if evalBexp b1 s then evalBexp b2 s else False
evalBexp (Or       b1 b2) s = if evalBexp b1 s then True else evalBexp b2 s



--AST of Comm
data Comm = Skip | Assn Variable Aexp | Comp Comm Comm | IfThenElse Bexp Comm Comm | WhileDo Bexp Comm
            | RepeatUntil Comm Bexp | ForFromToDo Variable Aexp Aexp Comm

instance Show Comm where
	show  Skip                   = "skip"
	show (Assn v a)              = show v ++ " := " ++ show a
	show (Comp c1 c2)            = show c1 ++ "; " ++ show c2
	show (IfThenElse b c1 c2)    = "if (" ++ show b ++ ") then {" ++ show c1 ++ "} else {" ++ show c2 ++ "}"
	show (WhileDo b c)           = "while (" ++ show b ++ ") do {" ++ show c ++ "}"
	show (RepeatUntil c b)       = "repeat {" ++ show c ++ "} until (" ++ show b ++ ")"
	show (ForFromToDo v a1 a2 c) = "for " ++ show v ++ " := " ++ show a1 ++ " to " ++ show a2 ++ " do {" ++ show c ++ "}"

--evaluation of Comm in one step
evalCommOneStep :: (Comm, State) -> (Comm, State)
evalCommOneStep ( Skip,                       s) = (Skip, s)
evalCommOneStep ((Assn (V i) a),              s) = (Skip, s') where s' = insert i n s; n = evalAexp a s
evalCommOneStep ((Comp Skip c),               s) = (c, s)
evalCommOneStep ((Comp nonSkip c),            s) = ((Comp c' c), s') where (c', s') = evalCommOneStep (nonSkip, s)
evalCommOneStep ( IfThenElse b c1 c2,         s) = if (evalBexp b s) then (c1, s) else (c2, s) 
evalCommOneStep ((WhileDo b c),               s) = (IfThenElse b (Comp c (WhileDo b c)) Skip, s)
evalCommOneStep ((RepeatUntil c b),           s) = (Comp c (IfThenElse b Skip (RepeatUntil c b)), s)
evalCommOneStep ((ForFromToDo (V i) a1 a2 c), s) = (Comp (Assn (V i) (Num m))
                                                         (IfThenElse (Or (LessThan (Var i) (Num n)) (Equal (Var i) (Num n)))
														             (Comp c (ForFromToDo (V i) (Add (Var i) (Num 1)) (Num n) c))
																	  Skip),
												   s)
                                                   where m = evalAexp a1 s; n = evalAexp a2 s

--evaluation of Comm in small steps presented in sequence
--this is for HW 4
--please wrap the function around with putStrLn when testing it in GHCi mode in terminal
--e.g. type in the command in terminal> putStrLn (evalCommSeq testProgram1 sampleState)
evalCommSeq :: Comm -> State -> String
evalCommSeq Skip s = "<COMMAND@ skip, STATE@ [" ++ show' s ++ "]>\n"
evalCommSeq c    s = "<COMMAND@ " ++ show c ++ ", STATE@ [" ++ show' s ++ "]> => \n\n" ++ evalCommSeq c' s'
                     where (c', s') = evalCommOneStep (c, s)

--evaluation of Comm
--this is for HW 2
--please wrap the function around with show' inside putStrLn when testing it in GHCi mode in terminal
--e.g. type in the command in terminal> putStrLn (show' (evalComm testProgram1 sampleState))
evalComm :: Comm -> State -> State
evalComm  Skip                       s = s
evalComm (Assn (V i) a)              s = let n = evalAexp a s in insert i n s
evalComm (Comp c1 c2)                s = evalComm c2 (evalComm c1 s)
evalComm (IfThenElse b c1 c2)        s = if evalBexp b s then evalComm c1 s else evalComm c2 s
evalComm (WhileDo b c)               s = evalComm (IfThenElse b (Comp c (WhileDo b c)) Skip) s
evalComm (RepeatUntil c b)           s = evalComm (Comp c (IfThenElse b Skip (RepeatUntil c b))) s
evalComm (ForFromToDo (V i) a1 a2 c) s = evalComm (Comp (Assn (V i) (Num m))
                                                        (IfThenElse (Or (LessThan (Var i) (Num n)) (Equal (Var i) (Num n)))
														   (Comp c (ForFromToDo (V i) (Add (Var i) (Num 1)) (Num n) c))
														    Skip)
												  ) s
                                         where m = evalAexp a1 s; n = evalAexp a2 s



--test cases
testProgram1 :: Comm
testProgram1 = Comp (Assn (V "x") (Num 1)) (WhileDo (LessThan (Var "x") (Num 5)) (Comp (Comp (Comp (Comp ((Assn (V "y") (Var "x"))) ((Assn (V "z") (Num 3)))) (ForFromToDo (V "u") (Num 1) (Var "z") (Assn (V "x") (Add (Var "x") (Var "x"))))) (IfThenElse (LessThan (Var "x") (Mul (Var "z") (Var "y"))) (Assn (V "x") (Add (Var "y") (Num 10))) (RepeatUntil (Assn (V "x") (Sub (Var "x") (Num 1))) (LessThan (Var "x") (Mul (Var "z") (Var "y")))))) Skip))

testProgram2 :: Comm
testProgram2 = ForFromToDo (V "u") (Var "y") (Mul (Var "x") (Var "z")) (Assn (V "x") (Mul (Var "x") (Num 2)))



--(integer) division, m divided by n
--TORESOLVE, error
dvs :: Int -> Int -> Int
dvs m n 
 | n /= 0    = quot m n
 | otherwise = error "Division by 0!"

--power (exponential), m^n
pow :: Int -> Int -> Int
pow m n
 | n == 0    = 1
 | n > 0     = m * (pow m (n - 1))
 | otherwise = error "Negative power!"

--factorial, n!
fac :: Int -> Int
fac n
 | n == 0 = 1
 | n > 0  = n * (fac (n - 1))
 | otherwise = error "Factorial on negative interger!"

--binomial coefficient, m choose n
bin :: Int -> Int -> Int
bin m n
 | m == 0, n == 0        = 1
 | m == 0, n > 0         = 0
 | m > 0, n >= 0, n <= m = (fac m) `quot` ((fac (m - n)) * (fac n))
 | m > 0, n > m          = 0
 | otherwise             = error "Binomial on negative integer(s)!"