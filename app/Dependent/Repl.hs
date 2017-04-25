module Main where

import Util.Repl
import Dependent.Parsing
import Dependent.TypeCheck

main :: IO ()
main = repl "π> " $ \input -> pure . either id id $ do
  parsed <- parseTerm input
  ty <- typeCheck [] parsed
  pure $ show (eval parsed) ++ " has type " ++ show ty
