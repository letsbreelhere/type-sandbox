module FOmega.Parsing where

import Control.Arrow (left)
import Control.Monad
import Data.String (fromString)
import FOmega.Types
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

parseTerm :: String -> Either String (Lam CapName Name)
parseTerm input = left parseErrorPretty $ parse (termParser <* eof) "REPL" input

parseType :: String -> Either String (LamType CapName)
parseType input = left parseErrorPretty $ parse (typeParser <* eof) "REPL" input

name :: Parser Name
name = fmap fromString . lexeme $ some lowerChar

capName :: Parser CapName
capName = fmap fromString . lexeme $ some upperChar

kindTable :: [[Operator Parser Kind]]
kindTable = [ [ infixR "->" KArr ] ]

typeTable :: [[Operator Parser (LamType CapName)]]
typeTable = [ [ InfixL (KApp <$ space) ]
            , [ infixR "->" TArr ]
            ]

termTable :: [[Operator Parser (Lam CapName Name)]]
termTable = [ [ Postfix (PairFirst <$ keyword ".1"), Postfix (PairSecond <$ keyword ".2"), InfixL (App <$ space) ] ]

infixR :: String -> (a -> a -> a) -> Operator Parser a
infixR k f = InfixR (f <$ keyword k)

termParser :: Parser (Lam CapName Name)
termParser = makeExprParser termInner termTable

termInner :: Parser (Lam CapName Name)
termInner = do
  t <- termInner'
  appTy <- many $ keyword "[" *> typeParser <* keyword "]"
  pure $ foldr (flip AppTy) t appTy

termInner' :: Parser (Lam CapName Name)
termInner' =
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

pair :: Parser (Lam CapName Name)
pair = do
  keyword "<"
  l <- termParser
  comma
  r <- termParser
  keyword ">"
  pure (MkPair l r)

bind :: Parser (Lam CapName Name)
bind = do
  keyword "bind"
  tvs <- sepBy1 tvWithSig comma
  dot
  body <- termParser
  pure $ foldr (\(tv, k) e -> AbsTy tv k e) body tvs

impl :: Parser (Lam CapName Name)
impl = do
  keyword "{*"
  ty <- typeParser
  comma
  l <- termParser
  keyword "}"
  keyword "as"
  (tv, kind) <- tvWithSig
  dot
  ty' <- typeParser
  pure (ImplEx ty l tv kind ty')

useEx :: Parser (Lam CapName Name)
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

termSig :: Parser (Name, LamType CapName)
termSig = do
  v <- name
  keyword ":"
  ty <- typeParser
  pure (v, ty)

lambda :: Parser (Lam CapName Name)
lambda = do
  keyword "lambda"
  vars <- sepBy1 termSig comma
  dot
  body <- termParser
  pure $ foldr (\(v, ty) e -> Abs v ty e) body vars

kindParser :: Parser Kind
kindParser = makeExprParser (Proper <$ keyword "*" <|> parens kindParser) kindTable <?> "kind signature"

typeParser :: Parser (LamType CapName)
typeParser = makeExprParser typeInner typeTable <?> "expression"

typeInner :: Parser (LamType CapName)
typeInner =
  BoolTy <$ keyword "bool" <|>
  UnitTy <$ keyword "unit" <|>
  pairType <|>
  forAll <|>
  exists <|>
  kLam <|>
  TVar <$> capName <|>
  parens typeParser

pairType :: Parser (LamType CapName)
pairType = do
  keyword "<"
  l <- typeParser
  comma
  r <- typeParser
  keyword ">"
  pure $ Pair l r

tvWithSig :: Parser (CapName, Kind)
tvWithSig = do
  tv <- capName
  keyword "::"
  k <- kindParser
  pure (tv, k)

tvBinder :: String -> Parser [(CapName, Kind)]
tvBinder kw = keyword kw *> sepBy1 tvWithSig comma <* dot

typeAbstraction :: String -> (CapName -> Kind -> LamType CapName -> LamType CapName) -> Parser (LamType CapName)
typeAbstraction kw q = do
  tvs <- tvBinder kw
  ty <- typeParser
  pure $ foldr (\(tv, k) acc -> q tv k acc) ty tvs

kLam :: Parser (LamType CapName)
kLam = typeAbstraction "with" KLam

forAll :: Parser (LamType CapName)
forAll = typeAbstraction "forall" Forall

exists :: Parser (LamType CapName)
exists = typeAbstraction "exists" Exists
