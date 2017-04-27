module FOmega.Parsing (parseTerm) where

import Parsing.Common
import FOmega.Types
import Types.Name
import Text.Megaparsec hiding (space)
import Text.Megaparsec.Expr
import Text.Megaparsec.String

parseTerm :: String -> Either String (Lam Name Name)
parseTerm = replParse termParser

kindTable :: [[Operator Parser Kind]]
kindTable = [ [ infixR "->" KArr ] ]

typeTable :: [[Operator Parser (LamType Name)]]
typeTable = [ [ InfixL (KApp <$ space) ]
            , [ infixR "->" TArr ]
            ]

termTable :: [[Operator Parser (Lam Name Name)]]
termTable = [ [ Postfix (PairFirst <$ keyword ".1"), Postfix (PairSecond <$ keyword ".2"), InfixL (App <$ space), Postfix (flip AppTy <$> appTy) ] ]
  where appTy = keyword "[" *> typeParser <* keyword "]"

infixR :: String -> (a -> a -> a) -> Operator Parser a
infixR k f = InfixR (f <$ keyword k)

termParser :: Parser (Lam Name Name)
termParser = makeExprParser termInner termTable

termInner :: Parser (Lam Name Name)
termInner =
  lambda <|>
  bind <|>
  Bool True <$ keyword "true" <|>
  Bool False <$ keyword "false" <|>
  Unit <$ keyword "unit" <|>
  keyword "snd" *> fmap PairSecond termParser <|>
  pair <|>
  impl <|>
  useEx <|>
  Var <$> name <|>
  parens termParser

pair :: Parser (Lam Name Name)
pair = do
  keyword "<"
  l <- termParser
  comma
  r <- termParser
  keyword ">"
  pure (MkPair l r)

bind :: Parser (Lam Name Name)
bind = do
  keyword "bind"
  tvs <- sepBy1 tvWithSig comma
  dot
  body <- termParser
  pure $ foldr (\(tv, k) e -> AbsTy tv k e) body tvs

impl :: Parser (Lam Name Name)
impl = do
  keyword "{*"
  ty <- typeParser
  comma
  l <- termParser
  keyword "}"
  keyword "as"
  ty' <- typeParser
  pure (ImplEx ty l ty')

useEx :: Parser (Lam Name Name)
useEx = do
  keyword "let"
  keyword "{"
  tv <- capName
  comma
  v <- name
  keyword "}"
  keyword "="
  implTerm <- termParser
  keyword "->"
  inTerm <- termParser
  pure (UseEx tv v implTerm inTerm)

termSig :: Parser (Name, LamType Name)
termSig = do
  v <- name
  keyword ":"
  ty <- typeParser
  pure (v, ty)

lambda :: Parser (Lam Name Name)
lambda = do
  keyword "lambda"
  vars <- sepBy1 termSig comma
  dot
  body <- termParser
  pure $ foldr (\(v, ty) e -> Abs v ty e) body vars

kindParser :: Parser Kind
kindParser = makeExprParser (Proper <$ keyword "*" <|> parens kindParser) kindTable <?> "kind signature"

typeParser :: Parser (LamType Name)
typeParser = makeExprParser typeInner typeTable <?> "expression"

typeInner :: Parser (LamType Name)
typeInner =
  BoolTy <$ keyword "bool" <|>
  UnitTy <$ keyword "unit" <|>
  pairType <|>
  forAll <|>
  exists <|>
  kLam <|>
  TVar <$> capName <|>
  parens typeParser

pairType :: Parser (LamType Name)
pairType = do
  keyword "<"
  l <- typeParser
  comma
  r <- typeParser
  keyword ">"
  pure $ Pair l r

tvWithSig :: Parser (Name, Kind)
tvWithSig = do
  tv <- capName
  keyword "::"
  k <- kindParser
  pure (tv, k)

tvBinder :: String -> Parser [(Name, Kind)]
tvBinder kw = keyword kw *> sepBy1 tvWithSig comma <* dot

typeAbstraction :: String -> (Name -> Kind -> LamType Name -> LamType Name) -> Parser (LamType Name)
typeAbstraction kw q = do
  tvs <- tvBinder kw
  ty <- typeParser
  pure $ foldr (\(tv, k) acc -> q tv k acc) ty tvs

kLam :: Parser (LamType Name)
kLam = typeAbstraction "with" KLam

forAll :: Parser (LamType Name)
forAll = typeAbstraction "forall" Forall

exists :: Parser (LamType Name)
exists = typeAbstraction "exists" Exists
