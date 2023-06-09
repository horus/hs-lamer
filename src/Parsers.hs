module Parsers (parse) where

import Control.Applicative (some, (<|>))
import Control.Monad (void)
import Data.Void (Void)
import Lambda (Expr (..))
import Text.Megaparsec
  ( ParseErrorBundle,
    Parsec,
    between,
    eof,
  )
import Text.Megaparsec qualified as P (parse)
import Text.Megaparsec.Char (char, lowerChar, space)
import Text.Megaparsec.Char.Lexer (lexeme)

type Parser = Parsec Void String

pVar :: Parser Expr
pVar = Var <$> ident

pAbs :: Parser Expr
pAbs = Abs <$> between lam dot var <*> pExpr
  where
    lam = symbol '\\' <|> symbol '\x3bb'
    var = ident
    dot = symbol '.'

pExpr :: Parser Expr
pExpr = foldl1 App <$> some (lex' pExpr')

pExpr' :: Parser Expr
pExpr' = pAbs <|> pVar <|> parens pExpr

lex' :: Parser a -> Parser a
lex' = lexeme space

ident :: Parser String
ident = (: []) <$> lex' lowerChar -- a string made of single lowercase char

symbol :: Char -> Parser ()
symbol = void . lex' . char

parens :: Parser Expr -> Parser Expr
parens = between (symbol '(') (symbol ')')

parse :: String -> Either (ParseErrorBundle String Void) Expr
parse = P.parse (pExpr <* eof) ""
