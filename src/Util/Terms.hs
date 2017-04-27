-- Misc. helper functions for terms.
module Util.Terms where

import Control.Monad (replicateM)
import Types.Variable

varMax :: (Foldable t, Ord a, Fresh a) => t a -> a
varMax = foldr max begin

rename :: (Functor f, Variable v) => f v -> (v, v) -> f v
rename fv (v, v') = fmap (\x -> if x == v then v' else x) fv

-- Rename `oldVar` in `outer` such that its new name doesn't occur in `inner`.
-- E.g., `freshen (Abs "x" (Var "x")) (Var "x") "x" (\v e -> Abs v UnitTy e)` = `("y", Abs "y" (Var "y"))`.
-- Used to make substitutions capture-avoiding.
freshen
  :: (Variable a, Functor f, Foldable f)
  => f a
  -> f a
  -> a
  -> (a, f a)
freshen inner outer oldVar =
  let freshVar = fresh (max (varMax outer) (varMax inner))
      outer' = rename outer (oldVar, freshVar)
   in (freshVar, outer')

prettyVars :: [String]
prettyVars = concatMap (`replicateM` ['a'..'z']) [1..]
