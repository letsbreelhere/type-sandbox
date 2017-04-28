module SystemF.Eval where

import Data.List (find)
import Data.Maybe (fromMaybe)
import SystemF.Types
import Types.Variable
import Util.Terms

eval :: (Variable a, Variable tv) => Lam tv a -> Lam tv a
eval = \case
  App l r ->
    let l' = eval l
        r' = eval r
     in case l' of
          Abs x _ t -> eval (substitute (x, r') (eval t))
          _ -> App l' r'
  Abs v ty e -> Abs v ty e
  Var v -> Var v
  TyCon tv -> TyCon tv
  AbsTy tv e -> AbsTy tv e
  AppTy e ty -> AppTy (eval e) ty
  Case inTerm clauses ->
    let (h:ts) = applicationList inTerm
        con = case h of
                TyCon c -> c
                _ -> error "Head of case input wasn't a tycon"
        (letVars, clause) = fromMaybe (error "Couldn't find matching clause") $ (\(_,vs,e) -> (vs,e)) <$> find (\(tv, _, _) -> tv == con) clauses
        substitutions = if length letVars == length ts then zip letVars ts else error "Case variables and matched clause didn't match in length"
     in eval (foldr substitute clause substitutions)

applicationList :: Lam tv a -> [Lam tv a]
applicationList (App l r) = applicationList l ++ [r]
applicationList t = [t]

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
  Case t clauses -> Case t $ map (\(tv, vs, e) -> (tv, vs, e) ) clauses -- TODO: Freshen correctly. We need to freshen for every element of `vs`.
