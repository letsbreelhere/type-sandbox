module Untyped.Eval where

import Untyped.Types

eval :: (Eq a, Ord a, Enum a) => Lam a -> Lam a
eval (App l r) =
  case eval l of
    Abs x t -> eval (substitute x (eval r) (eval t))
    l' -> App l' (eval r)
eval root = root

substitute :: (Eq a, Ord a, Enum a) => a -> Lam a -> Lam a -> Lam a
substitute x r = \case
  Var x'
    | x == x' -> r
    | otherwise -> Var x'
  App s t -> App (substitute x r s) (substitute x r t)
  Abs y t
    | x == y -> Abs x t
    | otherwise -> let freshVar = succ (max (maximum t) (maximum r))
                       t' = fmap (\v -> if v == y then freshVar else v) t
                    in Abs freshVar (substitute x r t')

betaReduce :: (Eq a, Ord a, Enum a) => Lam a -> Lam a
betaReduce l = case l of
  App (Abs x t) s -> substitute x s t
  _ -> l
