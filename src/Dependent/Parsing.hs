module Dependent.Parsing (parseCommand, Command(..)) where

import Dependent.Types
import Parsing.Common
import Types.Name (Name)
import Text.Megaparsec hiding (space)

parseCommand :: String -> Either String Command
parseCommand = replParse symbols commandParser
  where
    symbols = [ "Eval"
              , "Check"
              , "Context"
              , "Define"
              , "Assume"
              , ":="
              , ":"
              , "true"
              , "false"
              , "Bool"
              , "Unit"
              , "unit"
              , "Type"
              , "fun"
              , "=>"
              , "pi"
              ]

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
  v <- name <|> capName
  keyword ":="
  body <- termParser
  pure (Define v body)

assumption :: Parser Command
assumption = do
  keyword "Assume"
  uncurry Assume <$> termSig

termParser :: Parser (Term Name)
termParser =
  try (App <$> factor <*> termParser) <|>
  factor

factor =
  lambda <|>
  piType <|>
  Bool True <$ keyword "true" <|>
  Bool False <$ keyword "false" <|>
  BoolTy <$ keyword "Bool" <|>
  Unit <$ keyword "unit" <|>
  UnitTy <$ keyword "Unit" <|>
  Type 0 <$ keyword "Type" <|>
  Var <$> (name <|> capName) <|>
  parens termParser

termSig :: Parser (Name, Term Name)
termSig = do
  v <- name <|> capName
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
