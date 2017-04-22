module SystemF.TypeCheck where

import SystemF.Types

typeCheck :: (Eq a, Eq tv, Ord tv, Enum tv) => [(a, LamType tv)] -> Lam tv a -> Maybe (LamType tv)
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
    Arr arg res | arg == rightTy -> pure res
    _ -> Nothing
typeCheck cxt (AbsTy tv l) = Forall tv <$> typeCheck cxt l -- TODO: Check if tv occurs in cxt, freshen?
typeCheck cxt (AppTy e ty) = do
  tau <- typeCheck cxt e
  case tau of
    Forall tv generalType -> pure (generalType `applyTVar` (tv, ty))
    _ -> Nothing

applyTVar :: (Eq tv, Ord tv, Enum tv) => LamType tv -> (tv, LamType tv) -> LamType tv
applyTVar BoolTy _ = BoolTy
applyTVar UnitTy _ = UnitTy
applyTVar var@(TVar tv) (tv', ty)
  | tv == tv' = ty
  | otherwise = var
applyTVar (Arr l r) s = Arr (applyTVar l s) (applyTVar r s)
applyTVar (Forall tv ty) (tv', ty')
  | tv == tv' = Forall tv ty
  | otherwise = let freshVar = succ (max (maximum ty) (maximum ty'))
                    ty'' = fmap (\t -> if t == tv then freshVar else t) ty
                 in Forall freshVar (applyTVar ty'' (tv', ty'))
