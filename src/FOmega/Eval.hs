module FOmega.Eval where

import FOmega.Types
import FOmega.Util
import Types.Variable

eval :: (Variable a, Variable tv) => Lam tv a -> Lam tv a
eval root = case root of
  App l r -> betaReduce (App (eval l) (eval r))
  Abs v ty e -> Abs v ty e
  Var v -> Var v
  Bool b -> Bool b
  Unit -> Unit
  AbsTy tv k e -> AbsTy tv k e
  AppTy e ty -> case eval e of
                  AbsTy tv _ e' -> eval (applyTVarInTerm e' (tv, ty))
                  e' -> error $ "Tried to apply type to a non-abstraction: " ++ show e'
  MkPair l r -> MkPair (eval l) (eval r)
  PairFirst e -> case eval e of
                   MkPair f _ -> f
                   e' -> error $ "Tried to project on non-pair: " ++ show e'
  PairSecond e -> case eval e of
                   MkPair _ s -> s
                   e' -> error $ "Tried to project on non-pair: " ++ show e'
  ImplEx{} -> root
  UseEx{letTv, letVar, letTerm, inTerm} ->
    case eval letTerm of
      ImplEx{implType, implTerm, exType} ->
        case evalType exType of
          Exists exTv _ _ ->
            let implType' = rename implType (exTv, letTv)
             in eval $ substitute (letVar, implTerm) (applyTVarInTerm inTerm (letTv, implType'))
          _ -> error "Received non-existential type in pack"
      _ -> error "Received non-implementation term in unpack"
