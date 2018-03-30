module Parser where

import           Control.Monad
import           Debug.Trace
import           System.IO
import           Text.Parsec.Char
import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Expr
import           Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token    as Token
import Data.Functor.Identity

println msg = trace (show msg) $ return ()

seeNext :: Int -> Parser ()
seeNext n = do
  s <- getParserState
  let out = take n (stateInput s)
  println out

--
-- Grammar:
-- Prog ::= Stmt | /*begin*/ StmtSeq /*end*/
-- StmtSeq ::= Stmt | Stmt StmtSeq
--
-- Stmt ::= Assgn | If | While | /*pass*/
-- Assgn ::= id := Expr
-- If ::= if Expr Prog Prog
-- While ::= while Expr Prog
--
-- Expr ::=
--     id |
--     literal |
--     op Expr Expr
--
--
data Expr
  = Id String
  | Literal Integer
  | ExprOp Op
           Expr
           Expr
  deriving (Show)

data Op
  = Add
  | Subtract
  | Multiply
  | Divide
  deriving (Show)

data Stmt
  = Seq [Stmt]
  | Assign String
           Expr
  | If Expr
       Stmt
       Stmt
  | While Expr
          Stmt
  | Pass
  deriving (Show)

languageDef =
  emptyDef
    { Token.identStart = letter
    , Token.identLetter = alphaNum
    , Token.reservedNames = ["begin", "end", "if", "while", "pass"]
    , Token.reservedOpNames = ["+", "-", "*", "/", ":="]
    }

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer -- parses an identifier

reserved = Token.reserved lexer -- parses a reserved name

reservedOp = Token.reservedOp lexer -- parses an operator

integer = Token.integer lexer -- parses an integer

whiteSpace = Token.whiteSpace lexer -- parses whitespace


whileParser :: Parser Stmt
whileParser = whiteSpace >> statement

statement :: Parser Stmt
statement = sequenceOfStmt

sequenceOfStmt = do
  list <- (sepBy statement' newline)
  traceM("list :" ++ show list)
  return $
    if length list == 1
      then head list
      else Seq list

statement' :: Parser Stmt
statement' = progStmt <|> ifStmt <|> whileStmt <|> passStmt <|> assignStmt

progStmt :: Parser Stmt
progStmt = do
  reserved "begin"
  stmt <- statement
  reserved "end"
  return $ Seq [stmt]

ifStmt :: Parser Stmt
ifStmt = do
  reserved "if"
  cond <- expr
  prog1 <- statement
  prog2 <- statement
  return $ If cond prog1 prog2

-- this won't work, expression is not a condition
whileStmt :: Parser Stmt
whileStmt = do
  reserved "while"
  expression <- expr
  prog1 <- statement
  return $ While expression prog1

passStmt :: Parser Stmt
passStmt = reserved "pass" >> return Pass

assignStmt :: Parser Stmt
assignStmt = do
  id <- identifier
  reserved ":="
  expression <- expr
  seeNext 10
  return $ Assign id expression

expr :: Parser Expr
expr = buildExpressionParser operators term

operators =
  [ [ Infix (reservedOp "+" >> return (ExprOp Add)) AssocLeft
    , Infix (reservedOp "-" >> return (ExprOp Subtract)) AssocLeft
    ]
  , [ Infix (reservedOp "*" >> return (ExprOp Multiply)) AssocLeft
    , Infix (reservedOp "/" >> return (ExprOp Divide)) AssocLeft
    ]
  ]

term = liftM Id identifier <|> liftM Literal integer

parseString :: String -> Stmt
parseString str =
  case parse whileParser "" str of
    Left e  -> error $ show e
    Right r -> r

parseFile :: String -> IO Stmt
parseFile file = do
  program <- readFile file
  traceM("program: " ++ show program)
  case parse whileParser "" program of
    Left e  -> print e >> fail "parse error"
    Right r -> return r
