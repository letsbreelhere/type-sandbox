module SystemF.TypeCheck where

import SystemF.Types
import Types.Variable
import Util.Terms

typeCheck :: (Variable a, Variable tv) => [AdtDef tv] -> [(tv, LamType tv)] -> [(a, LamType tv)] -> Lam tv a -> Maybe (LamType tv)
typeCheck adts cons cxt = \case
  Var v -> lookup v cxt
  TyCon tv -> lookup tv cons
  Abs v ty e -> do
    resultTy <- typeCheck adts cons ((v,ty):cxt) e
    pure (Arr ty resultTy)
  App l r -> do
    leftTy <- typeCheck adts cons cxt l
    rightTy <- typeCheck adts cons cxt r
    case leftTy of
      Arr arg res | arg == rightTy -> pure res
      _ -> Nothing
  AbsTy tv l -> Forall tv <$> typeCheck adts cons cxt l -- TODO: Check if tv occurs in cxt, freshen?
  AppTy e ty -> do
    tau <- typeCheck adts cons cxt e
    case tau of
      Forall tv generalType -> pure (generalType `applyTVar` (tv, ty))
      _ -> Nothing

applyTVar :: (Variable tv) => LamType tv -> (tv, LamType tv) -> LamType tv
applyTVar (ADT n) _ = ADT n
applyTVar var@(TVar tv) (tv', ty)
  | tv == tv' = ty
  | otherwise = var
applyTVar (Arr l r) s = Arr (applyTVar l s) (applyTVar r s)
applyTVar (Forall tv ty) (tv', ty')
  | tv == tv' = Forall tv ty
  | otherwise = let (freshVar, ty'') = freshen ty' ty tv
                 in Forall freshVar (applyTVar ty'' (tv', ty'))
