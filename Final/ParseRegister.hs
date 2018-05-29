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

type Index = Int
--type Index = String
type Symbol = Char
type Alphabet = [Symbol]
type Label = Int
--type Label = String
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
  emptyDef { Token.identStart      = char 'R' <|> char 'L'
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

identifier = Token.identifier lexer -- parses an identifier
reserved   = Token.reserved   lexer -- parses a reserved name
reservedOp = Token.reservedOp lexer -- parses an operator
parens     = Token.parens     lexer -- parses surrounding parenthesis:
                                    --   parens p
                                    -- takes care of the parenthesis and
                                    -- uses p to parse what's inside them
integer    = Token.integer    lexer -- parses an integer
semi       = Token.semi       lexer -- parses a semicolon
whiteSpace = Token.whiteSpace lexer -- parses whitespace

--whileParser :: Parser Stmt
--whileParser = whiteSpace >> statement
--statement :: Parser Stmt
--statement =   parens statement
--          <|> sequenceOfStmt
--sequenceOfStmt =
--  do list <- (sepBy1 statement' semi)
--     -- If there's only one statement return it without using Seq.
--     return $ if length list == 1 then head list else Seq list
--statement' :: Parser Stmt
--statement' =   ifStmt
--           <|> addStmt
--           <|> subStmt
--           <|> haltStmt
--           <|> printStmt

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
     register  <- identifier -- TODO: read identifier "R0" into integer 0
     reserved "THEN"
     label1 <- identifier -- TOOD: read idintifer "L1" into integer 1
     reserved "ELSE"
     label2 <- identifier -- TOOD: same
     reserved "OR"
     label3 <- identifier -- TODO: same
     return $ BRANCH register [label1 label2 label3]

addInstr :: Parser Instr
addInstr =
  do reserved "LET"
     register <- identifier
     reservedOp "+="
     symbol <- identifier
     return $ ADD register symbol

subInstr :: Parser Instr
subInstr =
  do reserved "LET"
     register <- identifier
     reservedOp "-="
     symbol <- identifier
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


