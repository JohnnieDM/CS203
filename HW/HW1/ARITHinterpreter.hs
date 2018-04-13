---------------------
--Program: ARITHinterpreter.hs
--Authors: Johnnie Chang and Wei-Lin Wu
--On this homework, we worked together for 2 hours;
--each of us worked separately for 1 hour.
--Besides addition and multiplication,
--we implemented other common operations, namely,
--subtraction, division, exponentiation, factorial, and binomial coefficients, for additional features.
----------------------

--(integer) division, m divided by n
dvs :: Int -> Int -> Int
dvs m n = if n == 0 then -1 else quot m n

--power (exponential), m^n
pow :: Int -> Int -> Int
pow m n
 | n < 0  = -1
 | n == 0 = 1
 | n > 0  = m * (pow m (n - 1))

--factorial, n!
fac :: Int -> Int
fac n
 | n < 0  = -1
 | n == 0 = 1
 | n > 0  = n * (fac (n - 1))

--binomial coefficient, m choose n
bin :: Int -> Int -> Int
bin m n
 | m < 0  = -1
 | m == 0 = if n == 0 then 1 else 0
 | m > 0  = if (n >= 0 && n <= m) then (fac m) `quot` ((fac (m - n)) * (fac n)) else -1


--AST of ARITH
data ARITH = Num Int | Add ARITH ARITH | Sub ARITH ARITH | Mul ARITH ARITH
            | Div ARITH ARITH | Exp ARITH ARITH | Fac ARITH | Bin ARITH ARITH

--evaluation of ARITH
eval :: ARITH -> Int
eval (Num n)     = n
eval (Add e1 e2) = (eval e1) + (eval e2)
eval (Mul e1 e2) = (eval e1) * (eval e2)
eval (Sub e1 e2) = (eval e1) - (eval e2)
eval (Div e1 e2) = (eval e1) `dvs` (eval e2)
eval (Exp e1 e2) = pow (eval e1) (eval e2)
eval (Fac e)     = fac (eval e) 
eval (Bin e1 e2) = bin (eval e1) (eval e2)

--test 1 of eval: 2! ^ 4 * ((7 + 1 - 5) choose (6 / 3))
test1_eval :: Bool
test1_eval = let e = Mul (Exp (Fac (Num 2)) (Num 4)) (Bin (Sub (Add (Num 7) (Num 1)) (Num 5)) (Div (Num 6) (Num 3))) in
            (eval e) == 48

--test 2 of eval: 8 ^ 7 - 6! + C(5 * 3,  4 / 2 - 1)
test2_eval :: Bool
test2_eval = let e = Add (Sub (Exp (Num 8) (Num 7)) (Fac (Num 6))) (Bin (Mul (Num 5) (Num 3)) (Sub (Div (Num 4) (Num 2)) (Num 1))) in
            (eval e) == 2096447

--how ARITH expressions are evaluated and shown on the screen in GHCi mode; the function defined below is provided to allow more tests to be done in GHCi at your disposal
instance Show ARITH where
	show e = show (eval e)