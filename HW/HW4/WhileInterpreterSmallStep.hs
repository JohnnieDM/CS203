---------------------
-- Based on the feedback from TA's, we modified our implementation of states as a list of Int pairs rather than
-- a function from Variable to Int.

-- Program: WHILEinterpreter.hs
-- Authors: Johnnie Chang and Wei-Lin Wu
-- On this homework, we worked together for 5 hours;
-- Johnnie worked independently for 2.5 hours,
-- and Wei-Lin worked independently for 3 hours.
-- Besides skip, Assignment, composition, if then else, while do,
-- we implemented other common constructs, namely, repeat until, for from to do, as additional features.
----------------------


--Variable can be negatively indexed (to eliminate the use of constructor)
type Index = Int
data Variable = V Int deriving Eq
instance Show Variable where
	show (V n) = "V " ++ show n



type State = [(Int, Int)]

initialState :: State
initialState = []

sampleState :: State
sampleState = [(2, 5), (3, 3), (0, -5)]

query :: State -> Variable -> Int
query []       (V n) = error "Variable undefined"
query (x : xs) (V n) = if fst x == n then snd x else query xs (V n)

update :: State -> (Int, Int) -> State
update []       pair = [pair]
update (x : xs) pair = if fst x == fst pair then pair : xs else x : (update xs pair)

display :: State -> String
display [] = []
display ((m, n) : xs) = if xs == []
	                        then "(" ++ show (V m) ++ ") = " ++ show n
	                        else "(" ++ show (V m) ++ ") = " ++ show n ++ ", " ++ display xs



--AST of Aexp
data Aexp = Num Int | Var Index | Add Aexp Aexp | Sub Aexp Aexp | Mul Aexp Aexp
            | Div Aexp Aexp | Exp Aexp Aexp | Fac Aexp | Bin Aexp Aexp
			
instance Show Aexp where
	show (Num n)     = show n
	show (Var n)     = show (V n)
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
evalAexp (Var n)     s = query s (V n)
evalAexp (Add a1 a2) s = (evalAexp a1 s) + (evalAexp a2 s)
evalAexp (Mul a1 a2) s = (evalAexp a1 s) * (evalAexp a2 s)
evalAexp (Sub a1 a2) s = (evalAexp a1 s) - (evalAexp a2 s)
evalAexp (Div a1 a2) s = (evalAexp a1 s) `dvs` (evalAexp a2 s)
evalAexp (Exp a1 a2) s = pow (evalAexp a1 s) (evalAexp a2 s)
evalAexp (Fac a)     s = fac (evalAexp a s) 
evalAexp (Bin a1 a2) s = bin (evalAexp a1 s) (evalAexp a2 s)



--AST of Bexp
data Bexp = T | F | Equal Aexp Aexp | LessThan Aexp Aexp
            | Not Bexp | And Bexp Bexp | Or Bexp Bexp

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

--evaluation of Comm in small steps
evalCommOneStep :: (Comm, State) -> (Comm, State)
evalCommOneStep ( Skip,                       s) = (Skip, s)
evalCommOneStep ((Assn (V n) a),              s) = (Skip, s') where s' = update s (n, k); k = evalAexp a s
evalCommOneStep ((Comp Skip c),               s) = (c, s)
evalCommOneStep ((Comp nonSkip c),            s) = ((Comp c' c), s') where (c', s') = evalCommOneStep (nonSkip, s)
evalCommOneStep ( IfThenElse b c1 c2,         s) = if (evalBexp b s) then (c1, s) else (c2, s) 
evalCommOneStep ((WhileDo b c),               s) = (IfThenElse b (Comp c (WhileDo b c)) Skip, s)
evalCommOneStep ((RepeatUntil c b),           s) = (Comp c (IfThenElse (Not b) (RepeatUntil c b) Skip), s)
evalCommOneStep ((ForFromToDo (V n) a1 a2 c), s) = (Comp (Assn (V n) a1) (IfThenElse (Or (LessThan (Var n) a2) (Equal (Var n) a2)) (Comp c (Assn (V n) (Add (Var n) (Num 1)))) Skip), s)

evalCommSeq :: Comm -> State -> String
evalCommSeq Skip s = "<Skip, [" ++ display s ++ "]>\n"
evalCommSeq c    s = "<" ++ show c ++ ", [" ++ display s ++ "]> => \n\n" ++ evalCommSeq c' s'
                     where (c', s') = evalCommOneStep (c, s)

--evaluation of Comm
evalComm :: Comm -> State -> State
evalComm Skip                        s = s
evalComm (Assn (V n) a)              s = let v = evalAexp a s in update s (n, v)

evalComm (Comp c1 c2)                s = evalComm c2 (evalComm c1 s)
evalComm (IfThenElse b c1 c2)        s = if evalBexp b s then evalComm c1 s else evalComm c2 s
evalComm (WhileDo b c)               s = if evalBexp b s
	                                         then let s' = evalComm c s in evalComm (WhileDo b c) s'
										     else s
evalComm (RepeatUntil c b)           s = let s' = evalComm c s in
                                         if evalBexp b s' then s' else evalComm (RepeatUntil c b) s'
evalComm (ForFromToDo (V n) a1 a2 c) s = evalComm (WhileDo (Or (LessThan (Var n) (Num k)) (Equal (Var n) (Num k)))
                                                           (Comp c (Assn (V n) (Add (Var n) (Num 1)))))
											      s'
										 where s' = evalComm (Assn (V n) a1) s; k = evalAexp a2 s


--begin comment, below are test cases
test_program :: Comm
test_program = Comp (Assn (V 0) (Num 1)) (WhileDo (LessThan (Var 0) (Num 5)) (Comp (Comp (Comp (Comp ((Assn (V 1) (Var 0))) ((Assn (V 2) (Num 3)))) (ForFromToDo (V 3) (Num 1) (Var 2) (Assn (V 0) (Add (Var 0) (Var 0))))) (IfThenElse (LessThan (Var 0) (Mul (Var 2) (Var 1))) (Assn (V 0) (Add (Var 1) (Num 10))) (RepeatUntil (Assn (V 0) (Sub (Var 0) (Num 1))) (LessThan (Var 0) (Mul (Var 2) (Var 1)))))) Skip))

test_case1 :: Bool
test_case1 = query (evalComm test_program initialState) (V 0) == 5

test_case2 :: Bool
test_case2 = query (evalComm test_program initialState) (V 1) == 2

test_case3 :: Bool
test_case3 = query (evalComm test_program initialState) (V 2) == 3

test_case4 :: Bool
test_case4 = query (evalComm test_program initialState) (V 3) == 4
--end comment












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