module Main where

import Util.Repl
import FOmega.Eval
import FOmega.Parsing
import FOmega.TypeCheck

main :: IO ()
main = repl "ω> " $ \input -> pure . either id id $ do
  parsed <- parseTerm input
  ty <- typeCheck [] [] parsed
  let down = eval parsed
  pure $ show down ++ " has type " ++ show ty
