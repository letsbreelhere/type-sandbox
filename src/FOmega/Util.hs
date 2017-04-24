module FOmega.Util where

import FOmega.Types
import Types.Variable

varMax :: (Foldable t, Ord a, Enum a) => t a -> a
varMax = foldr max (toEnum 0)

rename :: (Functor f, Variable v) => f v -> (v, v) -> f v
rename fv (v, v') = fmap (\x -> if x == v then v' else x) fv

-- Given two expressions, replace a given variable with one not found in either expression.
-- E.g., `freshen (Abs "x" (Var "x")) (Var "x") "x" (\v e -> Abs v UnitTy e)` = `("y", Abs "y" (Var "y"))`.
-- Used to make substitutions capture-avoiding.
freshen
  :: (Variable a, Functor f, Foldable f)
  => f a
  -> f a
  -> a
  -> (a, f a)
freshen inner outer oldVar =
  let freshVar = succ (max (varMax outer) (varMax inner))
      outer' = rename outer (oldVar, freshVar)
   in (freshVar, outer')


substitute :: (Variable a) => (a, Lam tv a) -> Lam tv a -> Lam tv a
substitute (x, r) root = case root of
  Bool _ -> root
  Unit -> root
  Var x'
    | x == x' -> r
    | otherwise -> root
  App s t -> App (substitute (x, r) s) (substitute (x, r) t)
  Abs y ty t
    | x == y -> root
    | otherwise -> let (y', t') = freshen r t y
                    in Abs y' ty (substitute (x, r) t')
  AbsTy tv k e -> AbsTy tv k (substitute (x, r) e)
  AppTy e ty -> AppTy (substitute (x, r) e) ty
  MkPair one two -> MkPair (substitute (x,r) one) (substitute (x,r) two)
  PairFirst e -> PairFirst (substitute (x,r) e)
  PairSecond e -> PairSecond (substitute (x,r) e)
  ImplEx {implType, implTerm, exTv, exKind, exType} ->
    let implTerm' = substitute (x, r) implTerm
     in ImplEx implType implTerm' exTv exKind exType
  UseEx {letTv, letVar, letTerm, inTerm}
    | x == letVar -> root
    | otherwise -> let letTerm' = substitute (x, r) letTerm
                       inTerm' = substitute (x, r) inTerm
                    in UseEx letTv letVar letTerm' inTerm'

betaReduce :: (Variable a) => Lam tv a -> Lam tv a
betaReduce root = case root of
  App (Abs x _ t) s -> betaReduce (substitute (x, s) t)
  PairFirst (MkPair l _) -> betaReduce l
  PairSecond (MkPair _ r) -> betaReduce r
  _ -> root

applyTVar :: (Variable tv) => LamType tv -> (tv, LamType tv) -> LamType tv
applyTVar root p@(tv', ty') = case root of
  TVar tv
    | tv == tv'       -> ty'
    | otherwise       -> TVar tv
  BoolTy              -> BoolTy
  UnitTy              -> UnitTy
  TArr l r            -> TArr (applyTVar l p) (applyTVar r p)
  KApp l r            -> KApp (applyTVar l p) (applyTVar r p)
  Pair l r            -> Pair (applyTVar l p) (applyTVar r p)
  Forall tv kind ty   -> applyQuantifier Forall tv kind ty
  KLam tv kind ty     -> applyQuantifier KLam tv kind ty
  Exists tv kind ty   -> applyQuantifier Exists tv kind ty
  where
    applyQuantifier q tv kind ty =
      if tv == tv'
         then q tv kind ty
         else let (fresh, ty'') = freshen ty' ty tv
               in q fresh kind (applyTVar ty'' (tv', ty'))

applyTVarInTerm :: Variable tv => Lam tv a -> (tv, LamType tv) -> Lam tv a
applyTVarInTerm root pr = case root of
  Var _ -> root
  Bool _ -> root
  Unit -> root
  App s t -> App (applyTVarInTerm s pr) (applyTVarInTerm t pr)
  Abs x ty t -> Abs x (applyTVar ty pr) (applyTVarInTerm t pr)
  AbsTy tv k e
    | tv == fst pr -> AbsTy tv k e
    | otherwise -> AbsTy tv k (applyTVarInTerm e pr)
  AppTy e ty -> AppTy (applyTVarInTerm e pr) (applyTVar ty pr)
  MkPair one two -> MkPair (applyTVarInTerm one pr) (applyTVarInTerm two pr)
  PairFirst e -> PairFirst (applyTVarInTerm  e pr)
  PairSecond e -> PairSecond (applyTVarInTerm e pr)
  ImplEx{implType, implTerm, exTv, exKind, exType}
    | exTv == fst pr -> root
    | otherwise -> ImplEx (applyTVar implType pr) (applyTVarInTerm implTerm pr) exTv exKind (applyTVar exType pr)
  UseEx{letTv, letVar, letTerm, inTerm}
    | letTv == fst pr -> root
    | otherwise -> UseEx letTv letVar (applyTVarInTerm letTerm pr) (applyTVarInTerm inTerm pr)

evalType :: (Variable tv) => LamType tv -> LamType tv
evalType t = case t of
  BoolTy            -> BoolTy
  UnitTy            -> UnitTy
  TVar tv           -> TVar tv
  KLam tv kind ty   -> KLam tv kind (evalType ty)
  Forall tv kind ty -> Forall tv kind (evalType ty)
  Exists tv kind ty -> Exists tv kind (evalType ty)
  TArr l r          -> TArr (evalType l) (evalType r)
  Pair l r          -> Pair (evalType l) (evalType r)
  KApp l r          -> reduceType (KApp (evalType l) (evalType r))

reduceType :: (Variable tv) => LamType tv -> LamType tv
reduceType (KApp (KLam tv _ ty) r) = ty `applyTVar` (tv,r)
reduceType ty = ty
