module Types.Name where

import Data.String
import Types.Variable

data Name
  = BaseName String
  | SuccName String Int
  deriving (Eq)

instance Fresh Name where
  fresh (BaseName s) = SuccName s 1
  fresh (SuccName s n) = SuccName s (n + 1)
  begin = BaseName "x"

instance Show Name where
  show (BaseName s) = s
  show (SuccName s k) = s ++ show k

instance Ord Name where
  BaseName s <= BaseName s' = s <= s'
  SuccName s _ <= BaseName s' = s <= s'
  BaseName s <= SuccName s' _ = s <= s'
  SuccName s n <= SuccName s' n'
    | s == s' = n <= n'
    | otherwise = s <= s'

instance IsString Name where
  fromString = BaseName
