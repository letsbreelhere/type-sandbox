module FOmega.Parsing where

import Control.Arrow (left)
import Control.Monad
import Data.String (fromString)
import FOmega.Types
import Types.Name
import Text.Megaparsec hiding (space)
import Text.Megaparsec.Expr
import Text.Megaparsec.String
import Text.Megaparsec.Combinator
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

termParser :: Parser (Lam CapName Name)
termParser =
      lambda
  <|> app
  <|> bind
  <|> Bool True <$ keyword "true"
  <|> Bool False <$ keyword "false"
  <|> appTy
  <|> Var <$> name
  <|> parens termParser

appTy = do
  keyword "app"
  e <- termParser
  keyword "["
  ty <- typeParser
  keyword "]"
  pure (AppTy e ty)

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

app :: Parser (Lam CapName Name)
app = do
  keyword "`"
  App <$> termParser <*> termParser

kindTable = [ [ infixR "->" KArr ] ]
table = [ [ infixR "->" TArr ] ]

infixL name f = InfixL (f <$ keyword name)
infixR name f = InfixR (f <$ keyword name)

kindParser = makeExprParser (Proper <$ keyword "*" <|> parens kindParser) kindTable <?> "kind signature"

typeParser :: Parser (LamType CapName)
typeParser = makeExprParser typeInner table <?> "expression"

typeInner :: Parser (LamType CapName)
typeInner =
      BoolTy <$ keyword "bool"
  <|> UnitTy <$ keyword "unit"
  <|> forAll
  <|> TVar <$> capName
  <|> parens typeParser

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

tArr = do
  l <- typeParser
  keyword "->"
  r <- typeParser
  pure (TArr l r)
