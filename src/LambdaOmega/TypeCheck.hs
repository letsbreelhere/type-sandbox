module LambdaOmega.TypeCheck where

import LambdaOmega.Types
import Control.Monad (guard)

typeCheck :: (Eq a, Eq tv) => [(a, LamType tv)] -> Lam tv a -> Maybe (LamType tv)
typeCheck _ (Bool _) = pure BoolTy
typeCheck _ Unit = pure UnitTy
typeCheck cxt (Var v) = lookup v cxt
typeCheck cxt (Abs v ty e) = do
  Proper <- kindCheck [] ty
  resultTy <- typeCheck ((v,ty):cxt) e
  pure (TArr ty resultTy)
typeCheck cxt (App l r) = do
  TArr argTy resultTy <- typeCheck cxt l
  rightTy <- typeCheck cxt r
  guard (argTy == rightTy)
  pure resultTy

kindCheck :: (Eq tv) => [(tv, Kind)] -> LamType tv -> Maybe Kind
kindCheck _ BoolTy = pure Proper
kindCheck _ UnitTy = pure Proper
kindCheck cxt (TVar tv) = lookup tv cxt
kindCheck cxt (TArr l r) = do
  Proper <- kindCheck cxt l
  Proper <- kindCheck cxt r
  pure Proper
kindCheck cxt (KLam tv kind ty) = do
  resultKind <- kindCheck ((tv,kind):cxt) ty
  pure (KArr kind resultKind)
kindCheck cxt (KApp l r) = do
  KArr argKind resultKind <- kindCheck cxt l
  rightKind <- kindCheck cxt r
  guard (argKind == rightKind)
  pure resultKind
