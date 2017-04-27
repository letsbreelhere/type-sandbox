module Main where

import SystemF.Eval
import SystemF.Parsing
import SystemF.TypeCheck
import SystemF.Types
import Types.Name
import Util.Repl

primitives :: [AdtDef Name]
primitives =
  [ AdtDef "Unit" [("Unit", [])]
  , AdtDef "Bool" [("True", []), ("False", [])]
  , AdtDef "Nat"  [("Z", []), ("S", [ADT "Nat"])]
  ]

-- TODO: Generate these automatically from `primitives`
tyCons :: [(Name, LamType Name)]
tyCons =
  [ ("Unit", ADT "Unit")
  , ("True", ADT "Bool")
  , ("False", ADT "Bool")
  , ("Z", ADT "Nat")
  , ("S", ADT "Nat" `Arr` ADT "Nat")
  ]

main :: IO ()
main = repl "F> " $ \input -> pure . either id id $ do
  parsed <- parseTerm input
  ty <- maybe (Right (TVar "[typecheck failed]")) Right $ typeCheck primitives tyCons [] parsed
  pure $ show parsed ++ " has type " ++ show ty
