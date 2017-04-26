module Dependent.Parsing (parseCommand, Command(..)) where

import Control.Arrow (left)
import Control.Monad
import Data.String (fromString)
import Dependent.Types
import Types.Name (Name)
import Text.Megaparsec hiding (space)
import Text.Megaparsec.Expr
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as Lex

data Command
  = Assume Name (Term Name)
  | Define Name (Term Name)
  | Eval (Term Name)
  | Check (Term Name)
  | Context
  deriving (Show)

parseCommand :: String -> Either String Command
parseCommand input = left parseErrorPretty $ parse (commandParser <* eof) "REPL" input

commandParser :: Parser Command
commandParser =
  assumption <|>
  definition <|>
  keyword "Eval" *> (Eval <$> termParser) <|>
  keyword "Check" *> (Check <$> termParser) <|>
  Context <$ keyword "Context"

definition :: Parser Command
definition = do
  keyword "Define"
  v <- name
  keyword ":="
  body <- termParser
  pure (Define v body)

assumption :: Parser Command
assumption = do
  keyword "Assume"
  v <- name
  keyword ":"
  ty <- termParser
  pure (Assume v ty)

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
name = fmap fromString . lexeme $ some (lowerChar <|> upperChar)

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
