--divide
dvd :: Int -> Int -> Int
dvd m n = if n == 0 then -1 else quot m n

--power
pow :: Int -> Int -> Int
pow m n
 | n < 0  = -1
 | n == 0 = 1
 | n > 0  = m * (pow m (n - 1))
 
--factorial
fac :: Int -> Int
fac n
 | n < 0  = -1
 | n == 0 = 1
 | n > 0  = n * (fac (n - 1))

--binomial
bin :: Int -> Int -> Int
bin m n
 | m < 0  = -1
 | m == 0 = if n == 0 then 1 else 0
 | m > 0  = if (n >= 0 && n <= m) then (fac m) `quot` ((fac (m - n)) * (fac n)) else -1

data ARITH = Num Int | Add ARITH ARITH | Sub ARITH ARITH | Mul ARITH ARITH | Div ARITH ARITH | Exp ARITH ARITH | Fac ARITH | Bin ARITH ARITH

eval :: ARITH -> Int
eval (Num n)     = n
eval (Add e1 e2) = (eval e1) + (eval e2)
eval (Mul e1 e2) = (eval e1) * (eval e2)
eval (Sub e1 e2) = (eval e1) - (eval e2)
eval (Div e1 e2) = (eval e1) `dvd` (eval e2)
eval (Exp e1 e2) = pow (eval e1) (eval e2)
eval (Fac e)     = fac (eval e) 
eval (Bin e1 e2) = bin (eval e1) (eval e2)

test_eval :: Bool
test_eval = let e = Mul (Add (Num 3) (Num 5)) (Num 2) in
            (eval e) == 16
			
instance Show ARITH where
	show e = show (eval e)
