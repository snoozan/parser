-- Susan Lunn
-- skl1958@rit.edu

module Parser where

import           Control.Monad
import           Debug.Trace
import           System.IO

import           Data.Functor.Identity
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
  = Add
  | Subtract
  | Multiply
  | Divide
  deriving (Show)

data Stmt
  = Beg [Stmt]
  | Assign String
           Expr
  | If Expr
  | While Expr
          Stmt
  | Pass
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

simpleWhiteSpace = void $ (void $ takeWhile1P Nothing f)
  where
    f x = x == ' ' || x == '\t'

sc :: Parser () -- ‘sc’ stands for “space consumer”
sc = do
  traceM("attemtping to strip whiteSpace")
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

whileParser :: Parser Stmt
whileParser = between sc eof statement

statement :: Parser Stmt
statement = sequenceOfStmt

sequenceOfStmt = do
  list <- sepBy1 statement' wSpace 
  return $
    if length list == 1
      then head list
      else Beg list

statement' :: Parser Stmt
statement' = begStmt <|> ifStmt <|> whileStmt <|> passStmt <|> assignStmt <|> endStmt

begStmt :: Parser Stmt
begStmt = do
  rword "begin"
  void (space)
  stmt <- statement
  return $ Beg [stmt]

endStmt :: Parser Stmt
endStmt = do
  rword "end"
  void(space)
  return $ End 

ifStmt :: Parser Stmt
ifStmt = do
  rword "if"
  void (space)
  cond <- expr
  return $ If cond 

-- this won't work, expression is not a condition
whileStmt :: Parser Stmt
whileStmt = do
  rword "while"
  expression <- expr
  prog1 <- statement
  return $ While expression prog1

passStmt :: Parser Stmt
passStmt = do 
  rword "pass" 
  void (space)
  return $ Pass

assignStmt :: Parser Stmt
assignStmt = do
  traceM("assigning ")
  id <- identifier
  traceM ("id :" ++ show id)
  void (symbol ":=")
  expression <- expr
  traceM ("expr :" ++ show expression)
  return $ Assign id expression

expr :: Parser Expr
expr = liftM Id identifier <|> liftM Literal integer <|> operator

operator :: Parser Expr
operator = do
  op <- oneOf "+-*/"
  void(space)
  lhs <- expr
  rhs <- expr
  return $ ExprOp (convertOp op) lhs rhs
  where
    convertOp '+' = Add
    convertOp '*' = Multiply
    convertOp '-' = Subtract
    convertOp '/' = Divide

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
