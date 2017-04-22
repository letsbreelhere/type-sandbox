module Types.Variable where

type Variable v = (Eq v, Ord v, Enum v, Show v)
