module Util.Beautify where

import Data.Foldable
import Data.List (nub)
import Types.Variable
import Util.Terms

beautify :: (Functor t, Foldable t, Variable a) => [a] -> t a -> t a
beautify newVars t =
  let oldVars = nub (toList t)
   in foldr (flip rename) t (oldVars `zip` newVars)
