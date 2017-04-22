module SystemF.Eval where

import SystemF.Types

eval :: (Eq a, Ord a, Enum a) => Lam tv a -> Lam tv a
eval = \case
  App l r -> betaReduce (App (eval l) (eval r))
  Abs v ty e -> Abs v ty (eval e)
  Var v -> Var v
  Bool b -> Bool b
  Unit -> Unit
  AbsTy tv e -> AbsTy tv e
  AppTy e ty -> AppTy (eval e) ty

substitute :: (Eq a, Ord a, Enum a) => (a, Lam tv a) -> Lam tv a -> Lam tv a
substitute (x, r) = \case
  Var x'
    | x == x' -> r
    | otherwise -> Var x'
  App s t -> App (substitute (x, r) s) (substitute (x, r) t)
  Abs y ty t
    | x == y -> Abs x ty t
    | otherwise -> let freshVar = succ (max (maximum t) (maximum r))
                       t' = fmap (\v -> if v == y then freshVar else v) t
                    in Abs freshVar ty (substitute (x, r) t')
  Bool b -> Bool b
  Unit -> Unit
  AbsTy tv e -> AbsTy tv (substitute (x, r) e)
  AppTy e ty -> AppTy (substitute (x, r) e) ty

betaReduce :: (Eq a, Ord a, Enum a) => Lam tv a -> Lam tv a
betaReduce l = case l of
  App (Abs x _ t) s -> substitute (x, s) t
  _ -> l
