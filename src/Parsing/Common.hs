module Parsing.Common where

import Control.Arrow (left)
import Control.Monad
import Data.String (fromString)
import Types.Name
import Text.Megaparsec hiding (space)
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

replParse :: Parser a -> String -> Either String a
replParse parser = left parseErrorPretty . parse (parser <* eof) "REPL"

name :: Parser Name
name = fmap fromString . lexeme $ (:) <$> lowerChar <*> many (lowerChar <|> upperChar)

capName :: Parser Name
capName = fmap fromString . lexeme $ (:) <$> upperChar <*> many (lowerChar <|> upperChar)
