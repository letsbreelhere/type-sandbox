module Dependent.Parsing where

import Control.Arrow (left)
import Control.Monad
import Data.String (fromString)
import Dependent.Types
import Types.Name
import Text.Megaparsec hiding (space)
import Text.Megaparsec.Expr
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as Lex

space :: Parser ()
space = Lex.space (void spaceChar) (Lex.skipLineComment "--") (Lex.skipBlockComment "{-" "-}")

keyword :: String -> Parser ()
keyword k = void (lexeme (string k) <?> show k)

lexeme :: Parser a -> Parser a
lexeme = Lex.lexeme space

comma :: Parser ()
comma = keyword ","

dot :: Parser ()
dot = keyword "."

parens :: Parser a -> Parser a
parens p = keyword "(" *> p <* keyword ")"

name :: Parser Name
name = fmap fromString . lexeme $ some lowerChar

parseTerm :: String -> Either String (Term Name)
parseTerm input = left parseErrorPretty $ parse (termParser <* eof) "REPL" input

termTable :: [[Operator Parser (Term Name)]]
termTable = [ [ InfixL (App <$ space) ] ]

termParser :: Parser (Term Name)
termParser = makeExprParser termInner termTable

termInner :: Parser (Term Name)
termInner =
  lambda <|>
  piType <|>
  Bool True <$ keyword "true" <|>
  Bool False <$ keyword "false" <|>
  BoolTy <$ keyword "Bool" <|>
  Unit <$ keyword "unit" <|>
  UnitTy <$ keyword "Unit" <|>
  Type 0 <$ keyword "Type" <|>
  Var <$> name <|>
  parens termParser

termSig :: Parser (Name, Term Name)
termSig = do
  v <- name
  keyword ":"
  ty <- termParser
  pure (v, ty)

lambda :: Parser (Term Name)
lambda = do
  keyword "fun"
  vars <- sepBy1 termSig comma
  keyword "=>"
  body <- termParser
  pure $ foldr (\(v, ty) e -> Lambda v ty e) body vars

piType :: Parser (Term Name)
piType = do
  keyword "pi"
  vars <- sepBy1 termSig comma
  keyword "=>"
  body <- termParser
  pure $ foldr (\(v, ty) e -> Pi v ty e) body vars
