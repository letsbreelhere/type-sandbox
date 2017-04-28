module Main where

import Util.Repl
import Parsing.Common

main = repl "> " $ \input -> pure $ either id show (tokenize symbols input)
  where
    symbols = [ "Eval"
              , "Check"
              , "Context"
              , "Define"
              , "Assume"
              , ":="
              , ":"
              , "true"
              , "false"
              , "Bool"
              , "Unit"
              , "unit"
              , "Type"
              , "fun"
              , "=>"
              , "pi"
              ]
