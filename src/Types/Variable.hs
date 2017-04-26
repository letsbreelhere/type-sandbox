module Types.Variable where

class Fresh a where
  fresh :: a -> a
  begin :: a

type Variable v = (Eq v, Ord v, Fresh v, Show v)
