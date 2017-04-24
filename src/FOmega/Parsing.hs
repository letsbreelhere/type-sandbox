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
typeTable = [ [ infixR "->" TArr ] ]

termTable :: [[Operator Parser (Lam CapName Name)]]
termTable = [ [ InfixL (App <$ space) ] ]

infixR :: String -> (a -> a -> a) -> Operator Parser a
infixR k f = InfixR (f <$ keyword k)

termParser :: Parser (Lam CapName Name)
termParser = makeExprParser termInner termTable

termInner :: Parser (Lam CapName Name)
termInner = do
  t <- termInner'
  appTy <- optional $ keyword "[" *> typeParser <* keyword "]"
  case appTy of
    Nothing -> pure t
    Just ty -> pure (AppTy t ty)

termInner' :: Parser (Lam CapName Name)
termInner' =
  lambda <|>
  bind <|>
  Bool True <$ keyword "true" <|>
  Bool False <$ keyword "false" <|>
  Unit <$ keyword "unit" <|>
  keyword "fst" *> fmap PairFirst termParser <|>
  keyword "snd" *> fmap PairSecond termParser <|>
  pair <|>
  Var <$> name <|>
  parens termParser

pair :: Parser (Lam CapName Name)
pair = do
  keyword "<"
  l <- termParser
  keyword ","
  r <- termParser
  keyword ">"
  pure (MkPair l r)

bind :: Parser (Lam CapName Name)
bind = do
  keyword "bind"
  tvs <- sepBy1 tvWithSig (keyword ",")
  keyword "."
  body <- termParser
  pure $ foldr (\(tv, k) e -> AbsTy tv k e) body tvs

termSig :: Parser (Name, LamType CapName)
termSig = do
  v <- name
  keyword ":"
  ty <- typeParser
  pure (v, ty)

lambda :: Parser (Lam CapName Name)
lambda = do
  keyword "lambda"
  vars <- sepBy1 termSig (keyword ",")
  keyword "."
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
  forAll <|>
  TVar <$> capName <|>
  parens typeParser

tvWithSig :: Parser (CapName, Kind)
tvWithSig = do
  tv <- capName
  keyword "::"
  k <- kindParser
  pure (tv, k)

forAll :: Parser (LamType CapName)
forAll = do
  keyword "forall"
  tvs <- sepBy1 tvWithSig (keyword ",")
  keyword "."
  ty <- typeParser
  pure $ foldr (\(tv, k) acc -> Forall tv k acc) ty tvs