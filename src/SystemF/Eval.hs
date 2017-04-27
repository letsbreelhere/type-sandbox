module SystemF.Eval where

import SystemF.Types
import Types.Variable
import Util.Terms

eval :: (Variable a) => Lam tv a -> Lam tv a
eval = \case
  App l r ->
    let l' = eval l
        r' = eval r
     in case l' of
          Abs x _ t -> eval (substitute (x, r') (eval t))
          _ -> App l' r'
  Abs v ty e -> Abs v ty (eval e)
  Var v -> Var v
  TyCon tv -> TyCon tv
  AbsTy tv e -> AbsTy tv e
  AppTy e ty -> AppTy (eval e) ty

substitute :: (Variable a) => (a, Lam tv a) -> Lam tv a -> Lam tv a
substitute (x, r) = \case
  Var x'
    | x == x' -> r
    | otherwise -> Var x'
  App s t -> App (substitute (x, r) s) (substitute (x, r) t)
  Abs y ty t
    | x == y -> Abs x ty t
    | otherwise -> let (freshVar, t') = freshen r t y
                    in Abs freshVar ty (substitute (x, r) t')
  TyCon tv -> TyCon tv
  AbsTy tv e -> AbsTy tv (substitute (x, r) e)
  AppTy e ty -> AppTy (substitute (x, r) e) ty
