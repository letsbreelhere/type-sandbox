module SimplyTyped.Types where

data Lam a
  = Var a
  | App (Lam a) (Lam a)
  | Abs a LamType (Lam a)
  | Bool Bool
  | Unit
  deriving (Show, Functor, Foldable)

data LamType = BoolTy
             | UnitTy
             | Arr LamType LamType
             deriving (Eq)

instance Show LamType where
  showsPrec _ BoolTy = showString "Bool"
  showsPrec _ UnitTy = showString "Unit"
  showsPrec d (Arr l r) =
    showParen (d > arrPrec) $
    showsPrec (arrPrec + 1) l . showString " -> " . showsPrec d r
    where
      arrPrec = 10
