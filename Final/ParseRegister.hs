module ParseWhile where

import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

--data BExpr = BoolConst Bool
--           | Not BExpr
--           | BBinary BBinOp BExpr BExpr
--           | RBinary RBinOp AExpr AExpr
--            deriving (Show)
--data BBinOp = And | Or deriving (Show)
--data RBinOp = Greater | Less deriving (Show)

--data AExpr = Var String
--           | IntConst Integer
--           | Neg AExpr
--           | ABinary ABinOp AExpr AExpr
--             deriving (Show)

--data ABinOp = Add
--            | Subtract
--            | Multiply
--            | Divide
--              deriving (Show)

-- Character expression
data CExpr = Var Char

-- Register expression TODO: want "R"integer
data RExpr = 

data Stmt = Seq [Stmt]
--          | Assign String AExpr
          | Add RExpr Char
          | Sub RExpr Char
--          | If BExpr Stmt Stmt
          | IfEmpty RExpr Then Lbl Else Lbl Or Lbl
--          | While BExpr Stmt
--          | Skip
          | Print
          | Hault
            deriving (Show)

languageDef =
  emptyDef { --Token.commentStart    = "/*"
           --, Token.commentEnd      = "*/"
           --, Token.commentLine     = "//"
           , Token.identStart      = letter
           , Token.identLetter     = alphaNum
           , Token.reservedNames   = [ "IFEMPTY"
                                     , "THEN"
                                     , "ELSE"
                                     , "OR"
                                     , "LET"
           --                          , "while"
           --                          , "do"
           --                          , "skip"
           --                          , "true"
           --                          , "false"
           --                          , "not"
           --                          , "and"
           --                          , "or"
                                     ]
           , Token.reservedOpNames = [--"+", "-", "*", "/", ":="
                                      --, "<", ">", "and", "or", "not"
                                      "+=", "-="
                                     ]
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

whileParser :: Parser Stmt
whileParser = whiteSpace >> statement

statement :: Parser Stmt
statement =   parens statement
          <|> sequenceOfStmt

sequenceOfStmt =
  do list <- (sepBy1 statement' semi)
     -- If there's only one statement return it without using Seq.
     return $ if length list == 1 then head list else Seq list

statement' :: Parser Stmt
statement' =   ifStmt
--           <|> whileStmt
--           <|> skipStmt
--           <|> assignStmt
           <|> addStmt
           <|> subStmt

ifStmt :: Parser Stmt
ifStmt =
--  do reserved "if"
--     cond  <- bExpression
--     reserved "then"
--     stmt1 <- statement
--     reserved "else"
--     stmt2 <- statement
--     return $ If cond stmt1 stmt2
  do reserved "IFEMPTY"
     register  <- rExpression
     reserved "THEN"
     label1 <- statement
     reserved "ELSE"
     label2 <- statement
     reserved "OR"
     label3 <- statement
     return $ IfEmpty register Then label1 Else label2 Or label3

--whileStmt :: Parser Stmt
--whileStmt =
--  do reserved "while"
--     cond <- bExpression
--     reserved "do"
--     stmt <- statement
--     return $ While cond stmt

--assignStmt :: Parser Stmt
--assignStmt =
--  do var  <- identifier
--     reservedOp ":="
--     expr <- aExpression
--     return $ Assign var expr

addStmt :: Parser Stmt
addStmt =
  do reserved "LET"
     register <- rExpression
     reservedOp "+="
     char <- aExpression -- TODO: want char, not aExpression
     return $ Add register char

subStmt :: Parser Stmt
subStmt =
  do reserved "LET"
     register <- rExpression
     reservedOp "-="
     char <- aExpression
     return $ Sub register char

--skipStmt :: Parser Stmt
--skipStmt = reserved "skip" >> return Skip

haultStmt :: Parser Stmt
haultStmt = reserved "HAULT" >> return Hault

-- Register expression (not relational expression)
rExpression :: Parser RExpr
rExpression = buildExpressionParser "R" -- TODO how do I do this?...

--aExpression :: Parser AExpr
--aExpression = buildExpressionParser aOperators aTerm

--bExpression :: Parser BExpr
--bExpression = buildExpressionParser bOperators bTerm

--aOperators = [ [Prefix (reservedOp "-"   >> return (Neg             ))          ]
--             , [Infix  (reservedOp "*"   >> return (ABinary Multiply)) AssocLeft,
--                Infix  (reservedOp "/"   >> return (ABinary Divide  )) AssocLeft]
--             , [Infix  (reservedOp "+"   >> return (ABinary Add     )) AssocLeft,
--                Infix  (reservedOp "-"   >> return (ABinary Subtract)) AssocLeft]
--              ]

--bOperators = [ [Prefix (reservedOp "not" >> return (Not             ))          ]
--             , [Infix  (reservedOp "and" >> return (BBinary And     )) AssocLeft,
--                Infix  (reservedOp "or"  >> return (BBinary Or      )) AssocLeft]
--             ]

--aTerm =  parens aExpression
--     <|> liftM Var identifier
--     <|> liftM IntConst integer

--bTerm =  parens bExpression
--     <|> (reserved "true"  >> return (BoolConst True ))
--     <|> (reserved "false" >> return (BoolConst False))
--     <|> rExpression

--rExpression =
--  do a1 <- aExpression
--     op <- relation
--     a2 <- aExpression
--     return $ RBinary op a1 a2

--relation =   (reservedOp ">" >> return Greater)
--         <|> (reservedOp "<" >> return Less)

parseString :: String -> Stmt
parseString str =
  case parse whileParser "" str of
    Left e  -> error $ show e
    Right r -> r

parseFile :: String -> IO Stmt
parseFile file =
  do program  <- readFile file
     case parse whileParser "" program of
       Left e  -> print e >> fail "parse error"
       Right r -> return r


