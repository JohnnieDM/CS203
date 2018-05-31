module ParseWhile where

import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

import Data.HashMap

type Index = Integer
type Symbol = Char
type Alphabet = [Symbol]
type Label = Integer
type RegCont = String
type Input = RegCont
type Output = RegCont
type Program = [Instr]
type ProgCntr = Index
type RegMap = Map Index RegCont
type ProgCnt = Integer

data Instr = Seq [Instr]
           | ADD Index Symbol
           | SUB Index Symbol
           | BRANCH Index [Label]
           | PRINT
           | HALT
             deriving (Eq, Show)

alpha :: Alphabet
alpha = "ab"

position :: Symbol -> Alphabet -> Index
position s a = fromIntegral (head ([i | (i, char) <- zip [1 .. ] a, s == char] ++ [length a + 1]))

evalProg :: Program -> RegMap -> ProgCntr -> Output -> ((RegMap, ProgCntr), Output)
evalProg p rmap i o = case p !! fromInteger i of
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
                                            else evalProg p rmap (l !! fromIntegral (position (last reg) alpha)) o
                                        else evalProg p (insert k [] rmap) (l !! 0) o
                        PRINT -> if member 0 rmap
                                   then let reg = rmap ! 0 in evalProg p rmap (i + 1) (o ++ reg)
                                   else evalProg p (insert 0 "" rmap) (i + 1) o
                        HALT -> ((rmap, i + 1), o)

exec :: Program -> Input -> Output
exec p c = snd (evalProg p (insert 0 c empty) 0 "")

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
                                       --   parens p
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

--orSeires :: Parser [Label]
--orSeries =   label <- natural
--         <|> do label <- natural
--             reserved "OR"
--             orSeries
--
--ifInstr :: Parser Instr
--ifInstr =
--  do reserved "IFEMPTY"
--     registerIdent  <- identifier
--     let registerIndex = take (length registerIdent - 1) $ drop 1 $ registerIdent
--     let register = read registerIndex :: Integer
--     reserved "THEN"
--     label1 <- natural
--     reserved "ELSE"
--     something
--     return $ BRANCH register something

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
     return $ BRANCH register [label1, label2, label3]

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


