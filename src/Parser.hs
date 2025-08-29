module Parser (parseExpr, parseMutilExpr) where

import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.Scientific (toRealFloat)
import Data.Void (Void)
import Syntax (Expr (..))
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

sc :: Parser ()
sc =
  L.space
    space1
    (L.skipBlockComment "/*" "*/")
    (L.skipLineComment "//")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

number :: Parser Expr
number = ENum . toRealFloat <$> lexeme (L.signed sc L.scientific)

bool :: Parser Expr
bool = (EBool True <$ symbol "true") <|> (EBool False <$ symbol "false")

semicolon :: Parser ()
semicolon = symbol ";" *> pure ()

ifExpr :: Parser Expr
ifExpr = do
  _ <- symbol "if"
  cond <- expr
  _ <- symbol "then"
  bThen <- expr
  _ <- symbol "else"
  bElse <- expr
  return (EIf cond bThen bElse)

identifier :: Parser String
identifier = lexeme ((:) <$> letterChar <*> many alphaNumChar)

params :: Parser [String]
params = many identifier

funExpr :: Parser Expr
funExpr = do
  _ <- symbol "def"
  funcName <- identifier
  funcParams <- params
  _ <- symbol "=>"
  body <- expr
  return (EFun funcName funcParams body)

varExpr :: Parser Expr
varExpr = EVar <$> identifier

operators :: [[Operator Parser Expr]]
operators =
  [ [InfixL (EMul <$ symbol "*"), InfixL (EDiv <$ symbol "/")],
    [InfixL (EAdd <$ symbol "+"), InfixL (ESub <$ symbol "-")],
    [ InfixL (EEq <$ symbol "=="),
      InfixL (ENe <$ symbol "!="),
      InfixL (ELt <$ symbol "<"),
      InfixL (ELe <$ symbol "<="),
      InfixL (EGt <$ symbol ">"),
      InfixL (EGe <$ symbol ">=")
    ]
  ]

term :: Parser Expr
term = do
  t <- factor
  args <- many factor
  return $ foldl ECal t args

factor :: Parser Expr
factor = try binaryOp <|> primary

binaryOp :: Parser Expr
binaryOp = do
  left <- primary
  op <- operator
  right <- primary
  return $ op left right

operator :: Parser (Expr -> Expr -> Expr)
operator =
  (EAdd <$ symbol "+")
    <|> (ESub <$ symbol "-")
    <|> (EMul <$ symbol "*")
    <|> (EDiv <$ symbol "/")
    <|> (EEq <$ symbol "==")
    <|> (ENe <$ symbol "!=")
    <|> (ELt <$ symbol "<")
    <|> (ELe <$ symbol "<=")
    <|> (EGt <$ symbol ">")
    <|> (EGe <$ symbol ">=")

primary :: Parser Expr
primary = parens expr <|> number <|> bool <|> ifExpr <|> funExpr <|> varExpr

expr :: Parser Expr
expr = makeExprParser term operators

parseExpr :: String -> Either (ParseErrorBundle String Void) Expr
parseExpr = parse (sc *> expr <* eof) ""

parseMutilExpr :: String -> Either (ParseErrorBundle String Void) [Expr]
parseMutilExpr = parse (sc *> expr `sepBy1` semicolon <* eof) ""