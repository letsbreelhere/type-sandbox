module Untyped.Types where

data Lam a
  = Var a
  | App (Lam a) (Lam a)
  | Abs a (Lam a)
  deriving (Show, Functor, Foldable)
