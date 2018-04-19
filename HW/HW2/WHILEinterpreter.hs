--5hrs
--Variable can be negatively indexed (to eliminate the use of constructor)
type Index = Int
data Variable = V Int deriving Eq
type State = Variable -> Int

initialState :: State
initialState v = error "Variable uninitialized!"

nullState :: State
nullState v = 0

--AST of Aexp
--TORESOLVE: why I used deriving Eq
data Aexp = Num Int | Var Index | Add Aexp Aexp | Sub Aexp Aexp | Mul Aexp Aexp
            | Div Aexp Aexp | Exp Aexp Aexp | Fac Aexp | Bin Aexp Aexp
			deriving Eq

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
--TORESOLVE: why I used deriving Eq
data Bexp = T | F | Equal Aexp Aexp | LessThan Aexp Aexp
            | Not Bexp | And Bexp Bexp | Or Bexp Bexp
			deriving Eq

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


instance Show Variable where
	show (V n) = "V " ++ show n

--(integer) division, m divided by n
--TORESOLVE, error
dvs :: Int -> Int -> Int
dvs m n 
 | n /= 0    = quot m n
 | otherwise = error "Division by 0!"

--power (exponential), m^n
--TORESOLVE, error
pow :: Int -> Int -> Int
pow m n
 | n == 0    = 1
 | n > 0     = m * (pow m (n - 1))
 | otherwise = error "Negative power!"

--factorial, n!
--TORESOLVE, error
fac :: Int -> Int
fac n
 | n == 0 = 1
 | n > 0  = n * (fac (n - 1))
 | otherwise = error "Factorial on negative interger!"

--binomial coefficient, m choose n
--TORESOLVE, error
bin :: Int -> Int -> Int
bin m n
 | m == 0, n == 0        = 1
 | m == 0, n > 0         = 0
 | m > 0, n >= 0, n <= m = (fac m) `quot` ((fac (m - n)) * (fac n))
 | m > 0, n > m          = 0
 | otherwise             = error "Binomial on negative integer(s)!"