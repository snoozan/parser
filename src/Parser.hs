-- Susan Lunn
-- skl1958@rit.edu
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards    #-}

module Parser where

import           Control.Monad
import           Debug.Trace
import           System.IO

import           Data.Data
import           Data.Functor.Identity
import           Data.Maybe
import qualified Data.Text                  as Text
import qualified Data.Text.IO               as Text
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Text.Megaparsec.Expr

--
-- Grammar:
-- Prog ::= Stmt | /*begin*/ StmtSeq /*end*/
-- StmtSeq ::= Stmt | Stmt StmtSeq
--
-- Stmt ::= Assgn | If | While | /*pass*/ | begin StmtSeq end
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
  = ADD
  | SUB
  | MULT
  | DIV
  deriving (Show)

data Stmt
  = Seq [Stmt]
  | Assign String
           Expr
  | If Expr
  | While Expr
          Stmt
  | Pass
  | Beg Stmt
  | End
  deriving (Show)

-- languageDef =
--   emptyDef
--     { Token.identStart = letter
--     , Token.identLetter = alphaNumChar
--     , Token.symbolNames = ["begin", "end", "if", "while", "pass"]
--     , Token.symbolOpNames = ["+", "-", "*", "/", ":="]
--     }
type Parser = Parsec () String

-- we need to create our own white space consumer - we don't
-- want to eat newlines!
simpleWhiteSpace = void $ (void $ takeWhile1P Nothing f)
  where
    f x = x == ' ' || x == '\t'

sc :: Parser () -- ‘sc’ stands for “space consumer”
sc = do
  L.space simpleWhiteSpace lineComment blockComment
  where
    lineComment = L.skipLineComment "//"
    blockComment = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

integer :: Parser Integer
integer = lexeme L.decimal

wSpace :: Parser String
wSpace = symbol "\n"

rword :: String -> Parser ()
rword w = do
  (lexeme . try) (string w *> notFollowedBy alphaNumChar)

rws :: [String] -- list of reserved words
rws = ["if", "while", "pass", "begin", "end"]

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p = (:) <$> letterChar <*> many alphaNumChar
    check x =
      if x `elem` rws
        then fail $ "keyword " ++ show x ++ " cannot be an identifier"
        else return x

-- The actual parsing bits
-- using the space consumer parser and eof
-- both must succeed to succeed the parsing
whileParser :: Parser Stmt
whileParser = between sc eof statement

-- we can probably get rid of this, but
-- it makes it easier to read
statement :: Parser Stmt
statement = sequenceOfStmt

-- Returns a Parser Stmt
-- Seperate our statement by wSpace aka "\n"
-- but what if everything is on the same line?
-- :(
sequenceOfStmt = do
  list <- sepBy1 statement' wSpace
  return $
    if length list == 1
      then head list
      else Seq list

-- The possible statements defined below
statement' :: Parser Stmt
statement' =
  begStmt <|> ifStmt <|> whileStmt <|> passStmt <|> assignStmt <|> endStmt

begStmt :: Parser Stmt
begStmt = do
  rword "begin"
  void (space)
  stmt <- statement
  return $ Beg (Seq [stmt])

endStmt :: Parser Stmt
endStmt = do
  rword "end"
  void (space)
  return $ End

ifStmt :: Parser Stmt
ifStmt = do
  rword "if"
  void (space)
  cond <- expr
  return $ If cond

whileStmt :: Parser Stmt
whileStmt = do
  rword "while"
  expression <- expr
  let strExpr = showLiteral expression
  void (space)
  prog1 <- statement
  return $ While expression prog1

passStmt :: Parser Stmt
passStmt = do
  rword "pass"
  void (space)
  return $ Pass

assignStmt :: Parser Stmt
assignStmt = do
  id <- identifier
  void (symbol ":=")
  expression <- expr
  let strExpr = showLiteralOnly expression
  when (isJust strExpr) $ traceM ("PUSH " ++ (fromJust strExpr))
  traceM ("STORE " ++ id)
  return $ Assign id expression

-- if it's a literally immediate after the assign
-- it's safe to assume this is a push statement
showLiteralOnly :: Expr -> Maybe String
showLiteralOnly (Id a)                  = Nothing
showLiteralOnly (Literal a)             = Just (show a)
showLiteralOnly (ExprOp op expr1 expr2) = Nothing

-- Expressions: lift the Id and Literal from their respective monads
-- operator is already a self contained thing
expr :: Parser Expr
expr = liftM Id identifier <|> liftM Literal integer <|> operator

-- Pattern match our literal out
-- if it's an id, return nothong
-- if it's a literal, return it
-- if it's an expression, try the first expr for a literal or try to second
showLiteral :: Expr -> Maybe String
showLiteral (Id a)                  = Just ("LOAD " ++ a)
showLiteral (Literal a)             = Just ("PUSH " ++ show a)
showLiteral (ExprOp op expr1 expr2) = Nothing 

-- Pattern match out our id
--showId :: Expr -> Maybe String
--showId (Id a) = Just a
--showId (Literal a) = Nothing
--showId (ExprOp op expr1 expr2) = Nothing
operator :: Parser Expr
operator = do
  op <- oneOf "+-*/"
  void (space)
  lhs <- expr
  let strLhs = showLiteral lhs
  when (isJust strLhs) $ traceM (fromJust strLhs)
  rhs <- expr
  let strRhs = showLiteral rhs
  when (isJust strRhs) $ traceM (fromJust strRhs)
  traceM (show (convertOp op))
  return $ ExprOp (convertOp op) lhs rhs

convertOp :: Char -> Op
convertOp '+' = ADD
convertOp '*' = MULT
convertOp '-' = SUB
convertOp '/' = DIV

-- Some helper functions for parsing input
parseString :: String -> Stmt
parseString str =
  case parse whileParser "" str of
    Left e  -> error $ show e
    Right r -> r

parseFile :: String -> IO Stmt
parseFile file = do
  program <- readFile file
  case parse statement "" program of
    Left e  -> print e >> fail "parse error"
    Right r -> return r
