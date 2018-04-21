---------------------
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
type State = Variable -> Int

--sample state 1
initialState :: State
initialState v = error "Variable uninitialized!"

--sample state 2
nullState :: State
nullState v = 0

--AST of Aexp
data Aexp = Num Int | Var Index | Add Aexp Aexp | Sub Aexp Aexp | Mul Aexp Aexp
            | Div Aexp Aexp | Exp Aexp Aexp | Fac Aexp | Bin Aexp Aexp

--evaluation of Aexp
evalAexp :: Aexp -> State -> Int
evalAexp (Num n)     s = n
evalAexp (Var n)     s = s (V n)
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

--evaluation of Bexp (And, Or shortcircuited)
evalBexp :: Bexp -> State -> Bool
evalBexp T                s = True
evalBexp F                s = False
evalBexp (Equal    a1 a2) s = (evalAexp a1 s) == (evalAexp a2 s)
evalBexp (LessThan a1 a2) s = (evalAexp a1 s) < (evalAexp a2 s)
evalBexp (Not      b)     s = if evalBexp b s then False else True
evalBexp (And      b1 b2) s = if evalBexp b1 s then evalBexp b2 s else False
evalBexp (Or       b1 b2) s = if evalBexp b1 s then True else evalBexp b2 s

--AST of Comm
data Comm = Skip | Assn Variable Aexp | Comp Comm Comm | IfThenElse Bexp Comm Comm | WhileDo Bexp Comm
          | RepeatUntil Comm Bexp | ForFromToDo Variable Aexp Aexp Comm

--evaluation of Comm
evalComm :: Comm -> State -> State
evalComm Skip                    s = s
evalComm (Assn v a)              s = \u -> if u == v then evalAexp a s else s u
evalComm (Comp c1 c2)            s = evalComm c2 (evalComm c1 s)
evalComm (IfThenElse b c1 c2)    s = if evalBexp b s then evalComm c1 s else evalComm c2 s
evalComm (WhileDo b c)           s = if evalBexp b s
	                                     then let s' = evalComm c s in evalComm (WhileDo b c) s'
										 else s
evalComm (RepeatUntil c b)       s = let s' = evalComm c s in
                                     if evalBexp b s' then s' else evalComm (RepeatUntil c b) s'
evalComm (ForFromToDo (V n) a1 a2 c) s = evalComm (WhileDo (Or (LessThan (Var n) (Num k)) (Equal (Var n) (Num k)))
                                                           (Comp c (Assn (V n) (Add (Var n) (Num 1)))))
											      s' where
	                                     s' = evalComm (Assn (V n) a1) s; k = evalAexp a2 s

--begin comment, below are test cases
test_program :: Comm
test_program = Comp (Assn (V 0) (Num 1)) (WhileDo (LessThan (Var 0) (Num 5)) (Comp (Comp (Comp (Comp ((Assn (V 1) (Var 0))) ((Assn (V 2) (Num 3)))) (ForFromToDo (V 3) (Num 1) (Var 2) (Assn (V 0) (Add (Var 0) (Var 0))))) (IfThenElse (LessThan (Var 0) (Mul (Var 2) (Var 1))) (Assn (V 0) (Add (Var 1) (Num 10))) (RepeatUntil (Assn (V 0) (Sub (Var 0) (Num 1))) (LessThan (Var 0) (Mul (Var 2) (Var 1)))))) Skip))

test_case1 :: Bool
test_case1 = (evalComm test_program nullState) (V 0) == 5

test_case2 :: Bool
test_case2 = (evalComm test_program nullState) (V 1) == 2

test_case3 :: Bool
test_case3 = (evalComm test_program nullState) (V 2) == 3

test_case4 :: Bool
test_case4 = (evalComm test_program nullState) (V 3) == 4
--end comment

--make varible printable on the screen in GHCi mode
instance Show Variable where
	show (V n) = "V " ++ show n

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