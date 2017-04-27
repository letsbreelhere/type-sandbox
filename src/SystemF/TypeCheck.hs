module SystemF.TypeCheck where

import SystemF.Types

typeCheck :: (Eq a, Eq tv, Ord tv, Enum tv) => [(tv, [LamType tv])] -> [(a, LamType tv)] -> Lam tv a -> Maybe (LamType tv)
typeCheck adts cxt = \case
  Var v -> lookup v cxt
  TyCon tv -> foldr Arr (ADT tv) <$> lookup tv adts
  Abs v ty e -> do
    resultTy <- typeCheck adts ((v,ty):cxt) e
    pure (Arr ty resultTy)
  App l r -> do
    leftTy <- typeCheck adts cxt l
    rightTy <- typeCheck adts cxt r
    case leftTy of
      Arr arg res | arg == rightTy -> pure res
      _ -> Nothing
  AbsTy tv l -> Forall tv <$> typeCheck adts cxt l -- TODO: Check if tv occurs in cxt, freshen?
  AppTy e ty -> do
    tau <- typeCheck adts cxt e
    case tau of
      Forall tv generalType -> pure (generalType `applyTVar` (tv, ty))
      _ -> Nothing

applyTVar :: (Eq tv, Ord tv, Enum tv) => LamType tv -> (tv, LamType tv) -> LamType tv
applyTVar (ADT n) _ = ADT n
applyTVar var@(TVar tv) (tv', ty)
  | tv == tv' = ty
  | otherwise = var
applyTVar (Arr l r) s = Arr (applyTVar l s) (applyTVar r s)
applyTVar (Forall tv ty) (tv', ty')
  | tv == tv' = Forall tv ty
  | otherwise = let freshVar = succ (max (maximum ty) (maximum ty'))
                    ty'' = fmap (\t -> if t == tv then freshVar else t) ty
                 in Forall freshVar (applyTVar ty'' (tv', ty'))
