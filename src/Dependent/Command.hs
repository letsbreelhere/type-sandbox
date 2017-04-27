{-# LANGUAGE TemplateHaskell #-}
module Dependent.Command (process, ReplState, defaultReplState) where

import Control.Lens hiding (Context)
import Control.Monad.State
import Data.List (intercalate)
import Dependent.Parsing
import Dependent.TypeCheck
import Dependent.Types (Term(..))
import Types.Name
import qualified Util.Beautify as U
import Util.Terms

beautify :: Term Name -> Term Name
beautify = U.beautify (map BaseName prettyVars)

data ReplState = ReplState
  { _cxt :: [(Name, Term Name)]
  , _definitions :: [(Name, Term Name, Term Name)]
  }
makeLenses ''ReplState

defaultReplState :: ReplState
defaultReplState = ReplState [] []

process :: Command -> StateT ReplState IO String
process = \case
  Eval term -> do
    curCxt <- use cxt
    defnCxt <- map (\(n, ty, _) -> (n, ty)) <$> use definitions
    let fullCxt = curCxt ++ defnCxt
    curDefinitions <- reverse <$> use definitions
    pure $ case typeCheck fullCxt term of
      Left err -> err
      Right _ ->
        let letBoundTerm = foldr applyLet term curDefinitions
         in show $ beautify (eval letBoundTerm)
  Check term -> do
    curCxt <- use cxt
    curDefinitions <- map (\(n, ty, _) -> (n, ty)) <$> use definitions
    let fullCxt = curCxt ++ curDefinitions
    case typeCheck fullCxt term of
      Left err -> pure err
      Right ty -> pure (show ty)
  Assume n ty -> do
    cxt %= ((n, ty):)
    pure $ "Assumed " ++ show n ++ "."
  Define n term -> do
    curCxt <- use cxt
    case typeCheck curCxt term of
      Left err -> pure err
      Right ty -> do
        definitions %= ((n, ty, term):)
        pure $ "Defined " ++ show n ++ "."
  Context -> do
    curCxt <- use cxt
    curDefinitions <- use definitions
    let cxtLines = map (\(name, ty) -> show name ++ " : " ++ show ty) curCxt
        defnLines = map (\(name, ty, term) -> show name ++ " := " ++ show term ++ " : " ++ show ty) curDefinitions
    pure $ intercalate "\n" $ cxtLines ++ defnLines

-- Read as:
-- let v : ty = u in t == (λv : ty . t) u
applyLet :: (Name, Term Name, Term Name) -> Term Name -> Term Name
applyLet (v, ty, u) t = App (Lambda v ty t) u
