module Main where

import Util.Repl
import FOmega.Eval
import FOmega.Parsing
import FOmega.TypeCheck

main :: IO ()
main = repl "ω> " $ \input -> pure . either id id $ do
  parsed <- parseTerm input
  ty <- typeCheck [] [] parsed
  pure $ show (eval parsed) ++ " has type " ++ show ty
