module Dependent.Parsing (parseCommand, Command(..)) where

import Dependent.Types
import Parsing.Common
import Types.Name (Name)
import Text.Megaparsec hiding (space)
import Text.Megaparsec.Expr
import Text.Megaparsec.String

data Command
  = Assume Name (Term Name)
  | Define Name (Term Name)
  | Eval (Term Name)
  | Check (Term Name)
  | Context
  deriving (Show)

parseCommand :: String -> Either String Command
parseCommand = replParse commandParser

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
  uncurry Assume <$> termSig

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
