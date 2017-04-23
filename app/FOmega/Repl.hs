module Main where

import Util.Repl
import FOmega.TypeCheck
import FOmega.Parsing

main = repl "ω> " $ \input -> pure . either id id $ do
  parsed <- parseTerm input
  ty <- typeCheck [] [] parsed
  pure $ show parsed ++ " has type " ++ show ty
