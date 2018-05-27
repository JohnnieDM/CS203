import Data.HashMap

type Index = Int
type Symbol = Char
type Alphabet = [Symbol]
type Label = Int
type RegCont = String
type Input = RegCont
type Output = RegCont
data Instr = ADD Index Symbol | SUB Index Symbol | BRANCH Index [Label] | PRINT | HALT deriving (Eq)
type Program = [Instr]
type ProgCntr = Index
type RegMap = Map Index RegCont
type ProgCnt = Int

alpha :: Alphabet
alpha = "ab"

prog :: Program
prog = [BRANCH 0 [7, 1, 4], SUB 0 'a', ADD 1 'b', BRANCH 0 [7, 1, 4], SUB 0 'b', ADD 1 'a', BRANCH 0 [7, 1, 4], HALT]

rvrs :: Program
rvrs = [BRANCH 0 [7, 1, 4], SUB 0 'a', ADD 1 'a', BRANCH 0 [7, 1, 4], SUB 0 'b', ADD 1 'b', BRANCH 0 [7, 1, 4], BRANCH 1 [14, 8, 11], SUB 1 'a', ADD 2 'a', BRANCH 1 [14, 8, 11], SUB 1 'b', ADD 2 'b', BRANCH 1 [14, 8, 11], BRANCH 2 [21, 15, 18], SUB 2 'a', ADD 0 'a', BRANCH 2 [21, 15, 18], SUB 2 'b', ADD 0 'b', BRANCH 2 [21, 15, 18], HALT]

position :: Symbol -> Alphabet -> Index
position s a = head ([i | (i, char) <- zip [1 .. ] a, s == char] ++ [length a + 1])

evalProg :: Program -> RegMap -> ProgCntr -> Output -> (RegMap, ProgCntr)
evalProg p rmap i o = case p !! i of
                        ADD k s -> if member k rmap
                                     then evalProg p (insert k ((rmap ! k) ++ (s : [])) rmap) (i + 1) o
                                     else evalProg p (insert k (s : []) rmap) (i + 1) o
                        SUB k s -> if member k rmap
                                     then let reg = rmap ! k in
                                       if length reg > 0 && last reg == s
                                         then evalProg p (insert k (take (length reg - 1) reg) rmap) (i + 1) o
                                         else evalProg p rmap (i + 1) o
                                     else evalProg p (insert k (s : []) rmap) (i + 1) o
                        BRANCH k l -> if member k rmap
                                        then let reg = rmap ! k in
                                          if length reg == 0
                                            then evalProg p rmap (l !! 0) o
                                            else evalProg p rmap (l !! position (last reg) alpha) o
                                        else evalProg p (insert k [] rmap) (l !! 0) o
                        PRINT -> if member 0 rmap
                                   then let reg = rmap ! 0 in evalProg p rmap (i + 1) (o ++ reg)
                                   else evalProg p (insert 0 "" rmap) (i + 1) o 
                        HALT -> (rmap, i + 1)

exec :: Program -> Input -> Output
exec p c = (fst (evalProg p (insert 0 c empty) 0 "")) ! 0