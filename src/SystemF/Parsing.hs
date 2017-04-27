module SystemF.Parsing where

import Parsing.Common
import SystemF.Types
import Types.Name
import Text.Megaparsec hiding (space)
import Text.Megaparsec.Expr
import Text.Megaparsec.String

parseTerm :: String -> Either String (Lam Name Name)
parseTerm = replParse termParser

typeTable :: [[Operator Parser (LamType Name)]]
typeTable = [ [ infixR "->" Arr ] ]

termTable :: [[Operator Parser (Lam Name Name)]]
termTable = [ [ InfixL (App <$ space), Postfix (flip AppTy <$> appTy) ] ]
  where appTy = keyword "[" *> typeParser <* keyword "]"

infixR :: String -> (a -> a -> a) -> Operator Parser a
infixR k f = InfixR (f <$ keyword k)

termParser :: Parser (Lam Name Name)
termParser = makeExprParser termInner termTable

termInner :: Parser (Lam Name Name)
termInner =
  lambda <|>
  bind <|>
  caseParser <|>
  Var <$> name <|>
  TyCon <$> capName <|>
  parens termParser

caseParser :: Parser (Lam Name Name)
caseParser = do
  keyword "case"
  t <- Var <$> name
  keyword "{"
  clauses <- flip sepBy (keyword "|") $ do
    tycon <- capName
    args <- many name
    keyword "=>"
    result <- termParser
    pure (tycon, args, result)
  keyword "}"
  pure (Case t clauses)

termSig :: Parser (Name, LamType Name)
termSig = do
  v <- name
  keyword ":"
  ty <- typeParser
  pure (v, ty)

bind :: Parser (Lam Name Name)
bind = do
  keyword "bind"
  tvs <- sepBy1 capName comma
  dot
  body <- termParser
  pure $ foldr AbsTy body tvs

lambda :: Parser (Lam Name Name)
lambda = do
  keyword "lambda"
  vars <- sepBy1 termSig comma
  dot
  body <- termParser
  pure $ foldr (\(v, ty) e -> Abs v ty e) body vars

typeParser :: Parser (LamType Name)
typeParser = makeExprParser typeInner typeTable <?> "expression"

typeInner :: Parser (LamType Name)
typeInner =
  forAll <|>
  (keyword "@" *> fmap ADT capName) <|>
  TVar <$> capName <|>
  parens typeParser

forAll :: Parser (LamType Name)
forAll = typeAbstraction "forall" Forall

typeAbstraction :: String -> (Name -> LamType Name -> LamType Name) -> Parser (LamType Name)
typeAbstraction kw q = do
  tvs <- tvBinder kw
  ty <- typeParser
  pure $ foldr q ty tvs

tvBinder :: String -> Parser [Name]
tvBinder kw = keyword kw *> sepBy1 capName comma <* dot
