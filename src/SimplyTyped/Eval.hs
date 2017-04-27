module SimplyTyped.Eval where

import SimplyTyped.Types

eval :: (Eq a, Ord a, Enum a) => Lam a -> Lam a
eval (App l r) =
  case eval l of
    Abs x _ t -> eval (substitute x (eval r) (eval t))
    l' -> App l' (eval r)
eval root = root

substitute :: (Eq a, Ord a, Enum a) => a -> Lam a -> Lam a -> Lam a
substitute x r = \case
  Var x'
    | x == x' -> r
    | otherwise -> Var x'
  App s t -> App (substitute x r s) (substitute x r t)
  Abs y ty t
    | x == y -> Abs x ty t
    | otherwise -> let freshVar = succ (max (maximum t) (maximum r))
                       t' = fmap (\v -> if v == y then freshVar else v) t
                    in Abs freshVar ty (substitute x r t')
  Bool b -> Bool b
  Unit -> Unit
