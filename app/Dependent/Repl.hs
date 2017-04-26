module Main where

import Data.IORef
import Data.List (intercalate)
import Dependent.Parsing
import Dependent.TypeCheck
import Dependent.Types (Term(..))
import Types.Name
import Util.Repl

main :: IO ()
main = do
  cxt <- newIORef []
  defns <- newIORef []
  replLoop cxt defns

replLoop :: IORef [(Name, Term Name)] -> IORef [(Name, Term Name, Term Name)] -> IO ()
replLoop cxt defns = repl "π> " $
  either pure (processCommand cxt defns) . parseCommand

processCommand :: IORef [(Name, Term Name)] -> IORef [(Name, Term Name, Term Name)] -> Command -> IO String
processCommand cxt defns (Eval term) = do
  curCxt <- readIORef cxt
  curDefns <- reverse <$> readIORef defns
  pure $ case typeCheck curCxt term of
    Left err -> err
    Right _ ->
      -- TODO: this doesn't seem to be evaluating?
      let letBoundTerm = foldr applyLet term curDefns
       in show $ eval letBoundTerm
processCommand cxt _ (Check term) = do
  curCxt <- readIORef cxt
  case typeCheck curCxt term of
    Left err -> pure err
    Right ty -> pure (show ty)
processCommand cxt _ (Assume n ty) = do
  modifyIORef cxt ((n, ty):)
  pure $ "Assumed " ++ show n ++ "."
processCommand cxt defns (Define n term) = do
  curCxt <- readIORef cxt
  case typeCheck curCxt term of
    Left err -> pure err
    Right ty -> do
      modifyIORef defns ((n, ty, term):)
      modifyIORef cxt ((n, ty):)
      pure $ "Defined " ++ show n ++ "."
processCommand cxt defns Context = do
  curCxt <- readIORef cxt
  curDefns <- readIORef defns
  let cxtLines = map (\(name, ty) -> show name ++ " : " ++ show ty) curCxt
      defnLines = map (\(name, _, term) -> show name ++ " := " ++ show term) curDefns
  pure $ intercalate "\n" $ cxtLines ++ defnLines

applyLet :: (Name, Term Name, Term Name) -> Term Name -> Term Name
applyLet (v, ty, t') t = App (Lambda v ty t) t'
{-ty <- typeCheck [] parsed-}
{-pure $ show (eval parsed) ++ " has type " ++ show ty-}
