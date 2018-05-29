module ParseWhile where

import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

import Data.HashMap

--data Stmt = Seq [Stmt]
--          | Add RExpr Char
--          | Sub RExpr Char
--          | IfEmpty RExpr Then LExpr Else LExpr Or LExpr
--          | Print
--          | Halt
--            deriving (Show)

--type Index = Int
type Index = Integer
type Symbol = Char
type Alphabet = [Symbol]
--type Label = Int
type Label = Integer
type RegCont = String
type Input = RegCont
type Output = RegCont
type Program = [Instr]
type ProgCntr = Index
type RegMap = Map Index RegCont
type ProgCnt = Int

data Instr = Seq [Instr]
           | ADD Index Symbol
           | SUB Index Symbol
           | BRANCH Index [Label]
           | PRINT
           | HALT
             deriving (Eq)

languageDef =
  emptyDef { Token.identStart      = char 'R'
           , Token.identLetter     = alphaNum
           , Token.reservedNames   = [ "IFEMPTY"
                                     , "THEN"
                                     , "ELSE"
                                     , "OR"
                                     , "LET"
                                     ]
           , Token.reservedOpNames = ["+=", "-="]
           }

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier  lexer -- parses an identifier
reserved   = Token.reserved    lexer -- parses a reserved name
reservedOp = Token.reservedOp  lexer -- parses an operator
parens     = Token.parens      lexer -- parses surrounding parenthesis:
                                    --   parens p
                                    -- takes care of the parenthesis and
                                    -- uses p to parse what's inside them
integer    = Token.integer     lexer -- parses an integer
charLit    = Token.charLiteral lexer -- parses an char literal
semi       = Token.semi        lexer -- parses a semicolon
whiteSpace = Token.whiteSpace  lexer -- parses whitespace

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
             <|> addInstr
             <|> subInstr
             <|> haltInstr
             <|> printInstr

ifInstr :: Parser Instr
ifInstr =
  do reserved "IFEMPTY"
     --register  <- identifier -- TODO: read identifier "R0" into integer 0
     register  <- integer
     reserved "THEN"
     label1 <- integer
     reserved "ELSE"
     label2 <- integer
     reserved "OR"
     label3 <- integer
     return $ BRANCH register [label1, label2, label3]

addInstr :: Parser Instr
addInstr =
  do reserved "LET"
     --register <- identifier
     register <- integer
     reservedOp "+="
     symbol <- charLit
     return $ ADD register symbol

subInstr :: Parser Instr
subInstr =
  do reserved "LET"
     --register <- identifier
     register <- integer
     reservedOp "-="
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


