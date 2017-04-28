module Parsing.Common where

import Control.Arrow (left)
import Control.Monad (guard, void)
import qualified Data.Set as Set
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Char (isLower, isUpper)
import Data.String (fromString)
import Types.Name
import Text.Megaparsec hiding (Token, space, satisfy)
import Text.Megaparsec.Prim
import qualified Text.Megaparsec.String as String
import qualified Text.Megaparsec.Lexer as Lex
import Text.Megaparsec.Expr

type Parser = Parsec Dec TokenStream

newtype TokenStream = TokenStream [OneToken]
  deriving (Ord, Eq, Show)

data OneToken = OneToken { tokenLexeme :: Lexeme, srcPos :: SourcePos}
  deriving (Ord, Eq)

instance Show OneToken where
  show = show . tokenLexeme

instance Stream TokenStream where
  type Token TokenStream = OneToken
  uncons (TokenStream xs) = case xs of
                              [] -> Nothing
                              (y:ys) -> Just (y, TokenStream ys)
  updatePos _ _ _ t = (srcPos t, srcPos t)

instance ShowToken OneToken where
  showTokens (t :| ts) = show (tokenLexeme t) ++ ", " ++ intercalate "," (map show ts)

data Lexeme
  = Identifier String
  | Keyword String
  deriving (Eq, Ord, Show)

tokenizer :: [String] -> String.Parser TokenStream
tokenizer reserved = fmap TokenStream . many . lexeme $ OneToken <$> ((kw <?> "keyword") <|> (ident <?> "identifier")) <*> getPosition
  where
    kw = Keyword <$> choice (map string reserved)
    ident = Identifier <$> idString
    idString = (:) <$> letterChar <*> many (alphaNumChar <|> oneOf ("-_" :: String))

space :: String.Parser ()
space = Lex.space (void spaceChar) (Lex.skipLineComment "#") (Lex.skipBlockComment "{-" "-}")

lexeme :: String.Parser a -> String.Parser a
lexeme = Lex.lexeme space

keyword :: String -> Parser ()
keyword k = void (satisfy (== Keyword k)) <?> k

identifier :: Parser String
identifier = (<?> "identifier") $ do
  Identifier i <- satisfy isIdent
  pure i
  where
    isIdent (Identifier _) = True
    isIdent _ = False

satisfy :: (Lexeme -> Bool) -> Parser Lexeme
satisfy f = token testToken Nothing
  where
    testToken (OneToken x p) =
      if f x
        then Right x
        else Left (Set.singleton (Tokens (OneToken x p:|[])), Set.empty, Set.empty)

tokenize :: [String] -> String -> Either String TokenStream
tokenize kws = left parseErrorPretty . parse (tokenizer kws <* eof) "Lexer"

replParse' :: Parser a -> TokenStream -> Either String a
replParse' parser = left parseErrorPretty . parse (parser <* eof) "REPL"

replParse :: [String] -> Parser a -> String -> Either String a
replParse kws parser s = replParse' parser =<< tokenize kws s

comma :: Parser ()
comma = keyword ","

dot :: Parser ()
dot = keyword "."

parens :: Parser a -> Parser a
parens p = keyword "(" *> p <* keyword ")"

name :: Parser Name
name = (<?> "lower-case name") . fmap (\(Identifier i) -> fromString i) . satisfy $ \case
  Identifier i -> isLower (head i)
  _ -> False

capName :: Parser Name
capName = (<?> "upper-case name") . fmap (\(Identifier i) -> fromString i) . satisfy $ \case
  Identifier i -> isUpper (head i)
  _ -> False

infixR :: String -> (a -> a -> a) -> Operator Parser a
infixR k f = InfixR (f <$ keyword k)

infixL :: String -> (a -> a -> a) -> Operator Parser a
infixL k f = InfixL (f <$ keyword k)
