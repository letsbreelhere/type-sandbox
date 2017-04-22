module SimplyTyped.TypeCheck where

import SimplyTyped.Types

typeCheck :: (Eq a) => [(a, LamType)] -> Lam a -> Maybe LamType
typeCheck _ (Bool _) = pure BoolTy
typeCheck _ Unit = pure UnitTy
typeCheck cxt (Var v) = lookup v cxt
typeCheck cxt (Abs v ty e) = do
  resultTy <- typeCheck ((v,ty):cxt) e
  pure (Arr ty resultTy)
typeCheck cxt (App l r) = do
  leftTy <- typeCheck cxt l
  rightTy <- typeCheck cxt r
  case leftTy of
    Arr argTy resultTy | argTy == rightTy -> pure resultTy
    _ -> Nothing
