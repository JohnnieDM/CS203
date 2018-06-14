import System.Environment
import System.IO
import Control.Monad
import Data.HashMap
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token



----types for register machine----------------------------------------------------------
type Index    = Integer
type Symbol   = Char
type Alphabet = [Symbol]
type Label    = Index
type RegCont  = String              -- register content
type Input    = RegCont
type Output   = RegCont
type Program  = [Instr]
type RegArray = Map Index RegCont   -- register array
----------------------------------------------------------------------------------------



----instructions format-----------------------------------------------------------------
data Instr = Seq [Instr]            -- dummy instruction, for purpose of parsing
           | ADD Index Symbol
           | SUB Index Symbol
           | JUMP Index [Label]
           | PRINT
           | HALT
             deriving (Eq)

instance Show Instr where
  show (Seq list)             = ""  -- never used
  show (ADD index symbol)     = "LET R"     ++ show index ++ " += "   ++ show symbol
  show (SUB index symbol)     = "LET R"     ++ show index ++ " -= "   ++ show symbol
  show (JUMP index labellist) = "IFEMPTY R" ++ show index ++ " THEN " ++ show (labellist `labelNumbered` 0)
                                                          ++ " ELSE " ++ unrollRestOf labellist
  show  PRINT                 = "PRINT"
  show  HALT                  = "HALT"

-- auxiliary function used in the above function, show
unrollRestOf :: [Label] -> String
unrollRestOf labellist = unroll (tail labellist)

-- auxiliary function used in the above function, unrollRestOf
unroll :: [Label] -> String
unroll []       = ""
unroll (label : labels) = if labels == []
                            then show label
                            else show label ++ " OR " ++ unroll labels
----------------------------------------------------------------------------------------



----operations on registers------------------------------------------------------------
isEmpty :: RegCont -> Bool
isEmpty reg = reg == ""

notEmpty :: RegCont -> Bool
notEmpty reg = reg /= ""

lastSymbolOf :: RegCont -> Symbol
lastSymbolOf reg = last reg
----------------------------------------------------------------------------------------



---element-accessing functions for program isntructions, register array, and label list-
-- literally the program instruction labeled with label
instrLabeled :: Program -> Label -> Instr
program `instrLabeled` label = program !! fromInteger label

-- literally the register array element with index index
elemWithIndex :: RegArray -> Index -> RegCont
regarray `elemWithIndex` index = regarray ! index

-- literally the label numbered with number in label list
labelNumbered :: [Label] -> Integer -> Label
labellist `labelNumbered` number = labellist !! fromInteger number
----------------------------------------------------------------------------------------



----registers layout--------------------------------------------------------------------
-- conversion of register array contents into a string
showRegArray :: RegArray -> String
showRegArray regarray = display (toList regarray)

-- auxiliary function used in the above function, showRegArray
display :: [(Index, RegCont)] -> String
display []                   = ""
display ((index, content) : remainder) = if remainder == []
                                           then "R" ++ show index ++ " = \"" ++ content ++ "\""
                                           else "R" ++ show index ++ " = \"" ++ content ++ "\", "
                                                    ++ display remainder
----------------------------------------------------------------------------------------



----alphabet-related functions----------------------------------------------------------
-- fixed alphabet in this project, needs to be refined to allow user-specified alphabet
alphabet :: Alphabet
alphabet = "ab"

-- position of symbol in the alphabet
posInAlphabetOf :: Symbol -> Index
posInAlphabetOf symbol = fromIntegral (head ([index | (index, character) <- zip [1 .. ] alphabet, symbol == character] ++ [length alphabet + 1]))
----------------------------------------------------------------------------------------



----program evaluation and execution (big step)-----------------------------------------
evalProg :: Program -> RegArray -> Label -> Output -> ((RegArray, Label), Output)
evalProg program regarray label output =
  case program `instrLabeled` label of
    ADD ind symb -> if member ind regarray
                      then evalProg (program)
                                    (insert ind ((regarray `elemWithIndex` ind) ++ (symb : [])) regarray)
                                    (label + 1)
                                    (output)
                      else evalProg (program)
                                    (insert ind (symb : []) regarray)
                                    (label + 1)
                                    (output)
    SUB ind symb -> if member ind regarray
                      then let reg = regarray `elemWithIndex` ind in
                                       if notEmpty reg && lastSymbolOf reg == symb
                                         then evalProg (program)
                                                       (insert ind (take (length reg - 1) reg) regarray)
                                                       (label + 1)
                                                       (output)
                                         else evalProg (program)
                                                       (regarray)
                                                       (label + 1)
                                                       (output)
                      else evalProg (program)
                                    (insert ind [] regarray)
                                    (label + 1)
                                    (output)
    JUMP ind labels -> if member ind regarray
                         then let reg = regarray `elemWithIndex` ind in
                           if isEmpty reg
                             then evalProg (program)
                                           (regarray)
                                           (labels `labelNumbered` 0)
                                           (output)
                             else evalProg (program)
                                           (regarray)
                                           (labels `labelNumbered` posInAlphabetOf (lastSymbolOf reg))
                                           (output)
                         else evalProg (program)
                                       (insert ind [] regarray)
                                       (labels `labelNumbered` 0)
                                       (output)
    PRINT -> if member 0 regarray
               then let reg0 = regarray `elemWithIndex` 0 in
                    evalProg (program)
                             (regarray)
                             (label + 1)
                             (output ++ reg0)
               else evalProg (program)
                             (insert 0 "" regarray)
                             (label + 1)
                             (output)
    HALT -> ((regarray, label + 1), output)


exec :: Program -> Input -> Output
exec program input = snd (evalProg program (insert 0 input empty) 0 "")
----------------------------------------------------------------------------------------




----program evaluation and execution (small step)---------------------------------------
evalInstr :: Instr -> RegArray -> Label -> Output -> ((RegArray, Label), Output)
evalInstr instruction regarray label output =
  case instruction of
    ADD ind symb -> if member ind regarray
                      then (((insert ind ((regarray `elemWithIndex` ind) ++ (symb : [])) regarray), label + 1), output)
                      else (((insert ind (symb : []) regarray), label + 1), output)
    SUB ind symb -> if member ind regarray
                      then let reg = regarray `elemWithIndex` ind in
                        if notEmpty reg && lastSymbolOf reg == symb
                          then (((insert ind (take (length reg - 1) reg) regarray), label + 1), output)
                          else ((regarray, label + 1), output)
                      else (((insert ind [] regarray), label + 1), output)
    JUMP ind labels -> if member ind regarray
                         then let reg = regarray `elemWithIndex` ind in
                           if isEmpty reg
                             then ((regarray, (labels `labelNumbered` 0)), output)
                             else ((regarray, (labels `labelNumbered` posInAlphabetOf (lastSymbolOf reg))), output)
                         else (((insert ind [] regarray), (labels `labelNumbered` 0)), output)
    PRINT    -> if member 0 regarray
                  then let reg0 = regarray `elemWithIndex` 0 in ((regarray, label + 1), output ++ reg0)
                  else (((insert 0 [] regarray), label + 1), output)
    HALT     -> ((regarray, label), output)

type Trace = String

evalProgStepwise :: Program -> RegArray -> Label -> Output -> Trace
evalProgStepwise program regarray label output =
  case program `instrLabeled` label of
    ADD ind symb    -> let ((newregarray, nextlabel), newoutput) = evalInstr (ADD ind symb) regarray label output in
                         "Execute " ++ show label ++ " " ++ show (ADD ind symb) ++
                         "\nwith register(s): " ++ showRegArray regarray ++
                         "\n\n" ++ evalProgStepwise program newregarray nextlabel newoutput
    SUB ind symb    -> let ((newregarray, nextlabel), newoutput) = evalInstr (SUB ind symb) regarray label output in
                         "Execute " ++ show label ++ " " ++ show (SUB ind symb) ++
                         "\nwith register(s): " ++ showRegArray regarray ++
                         "\n\n" ++ evalProgStepwise program newregarray nextlabel newoutput
    JUMP ind labels -> let ((newregarray, nextlabel), newoutput) = evalInstr (JUMP ind labels) regarray label output in
                         "Execute " ++ show label ++ " " ++ show (JUMP ind labels) ++
                         "\nwith register(s): " ++ showRegArray regarray  ++
                         "\n\n" ++ evalProgStepwise program newregarray nextlabel newoutput
    PRINT           -> let ((newregarray, nextlabel), newoutput) = evalInstr PRINT regarray label output in
                         "Execute " ++ show label ++ " " ++ show PRINT ++
                         "\nwith register(s): " ++ showRegArray regarray ++
                         "\n\nCurrent output: \"" ++ newoutput ++ "\"\n\n" ++
                         evalProgStepwise program newregarray nextlabel newoutput
    HALT            -> let ((newregarray, nextlabel), newoutput) = evalInstr HALT regarray label output in
                         "Execute " ++ show label ++ " " ++ show HALT ++
                         "\nwith register(s): " ++ showRegArray regarray ++
                         "\n\n------------------\n" ++ "Program terminated\nwith register(s): " ++ showRegArray newregarray ++
                         "\n" ++ "and output: \"" ++ newoutput ++ "\"\n"

execStepwise :: Program -> Input -> Trace
execStepwise program input = evalProgStepwise program (insert 0 input empty) 0 ""
----------------------------------------------------------------------------------------


----the main function in this project---------------------------------------------------
main =
  do [stepwise, file, input] <- getArgs
     program <- readFile file
     case parse rmParser "" program of
       Left e  -> print e >> fail "parse error"
       Right r -> case r of
                    ADD _ _  -> putStrLn "" >> fail "No HALT!"
                    SUB _ _  -> putStrLn "" >> fail "No HALT!"
                    JUMP _ _ -> putStrLn "" >> fail "No HALT!"
                    PRINT    -> putStrLn "" >> fail "No HALT!"
                    HALT     -> if stepwise == "-s"
                                  then putStrLn "\nProgram trace\n-------------\n" >>
                                       putStrLn (execStepwise [HALT] input)
                                  else putStrLn (exec [HALT] input)
                    Seq s    -> if stepwise == "-t"
                                  then putStrLn "\nProgram trace\n-------------\n" >>
                                       putStrLn (execStepwise s input)
                                  else putStrLn (exec s input)
----------------------------------------------------------------------------------------



----parsing-----------------------------------------------------------------------------
languageDef =
  emptyDef { Token.identStart      = letter
           , Token.identLetter     = alphaNum
           , Token.reservedNames   = [ "IFEMPTY"
                                     , "THEN"
                                     , "ELSE"
                                     , "OR"
                                     , "LET"
                                     , "+="
                                     , "-="
                                     ]
           }

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier    lexer -- parses an identifier
reserved   = Token.reserved      lexer -- parses a reserved name
parens     = Token.parens        lexer -- parses surrounding parenthesis:
                                       -- parens p
                                       -- takes care of the parenthesis and
                                       -- uses p to parse what's inside them
integer    = Token.integer       lexer -- parses an integer
natural    = Token.natural       lexer -- parses a natural
charLit    = Token.charLiteral   lexer -- parses an char literal
stringLit  = Token.stringLiteral lexer -- parses an char literal
semi       = Token.semi          lexer -- parses a semicolon
whiteSpace = Token.whiteSpace    lexer -- parses whitespace

rmParser :: Parser Instr
rmParser = whiteSpace >> instruction

instruction :: Parser Instr
instruction =  sequenceOfInstr

sequenceOfInstr =
  do list <- (sepBy1 instruction' semi)
     -- If there's only one statement return it without using Seq.
     return $ if length list == 1 then head list else Seq list

instruction' :: Parser Instr
instruction' =   ifInstr
             <|> try subInstr
             <|> addInstr
             <|> haltInstr
             <|> printInstr

ifInstr :: Parser Instr
ifInstr =
  do reserved "IFEMPTY"
     registerIdent  <- identifier
     let registerIndex = take (length registerIdent - 1) $ drop 1 $ registerIdent
     let register = read registerIndex :: Integer
     reserved "THEN"
     label1 <- natural
     reserved "ELSE"
     label2 <- natural
     reserved "OR"
     label3 <- natural
     return $ JUMP register [label1, label2, label3]

addInstr :: Parser Instr
addInstr =
  do reserved "LET"
     registerIdent  <- identifier
     let registerIndex = take (length registerIdent - 1) $ drop 1 $ registerIdent
     let register = read registerIndex :: Integer
     reserved "+="
     symbol <- charLit
     return $ ADD register symbol

subInstr :: Parser Instr
subInstr =
  do reserved "LET"
     registerIdent  <- identifier
     let registerIndex = take (length registerIdent - 1) $ drop 1 $ registerIdent
     let register = read registerIndex :: Integer
     reserved "-="
     symbol <- charLit
     return $ SUB register symbol

haltInstr :: Parser Instr
haltInstr = reserved "HALT" >> return HALT

printInstr :: Parser Instr
printInstr = reserved "PRINT" >> return PRINT

parseString :: String -> Instr
parseString str =
  case parse rmParser "" str of
    Left e  -> error $ show e
    Right r -> r

parseFile :: String -> IO Instr
parseFile file =
  do program  <- readFile file
     case parse rmParser "" program of
       Left e  -> print e >> fail "parse error"
       Right r -> return r
----------------------------------------------------------------------------------------
