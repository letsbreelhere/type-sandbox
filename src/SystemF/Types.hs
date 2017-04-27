module SystemF.Types where

data Lam tv a
  = Var a
  | TyCon tv
  | App (Lam tv a) (Lam tv a)
  | Abs a (LamType tv) (Lam tv a)
  | AbsTy tv (Lam tv a)
  | AppTy (Lam tv a) (LamType tv)
  deriving (Show, Functor, Foldable)

data LamType tv
  = ADT tv
  | TVar tv
  | Arr (LamType tv) (LamType tv)
  | Forall tv (LamType tv)
  deriving (Eq, Functor, Foldable)

arrPrec :: Int
arrPrec = 10

forallPrec :: Int
forallPrec = 8

instance Show tv => Show (LamType tv) where
  showsPrec _ (ADT n) = shows n
  showsPrec _ (TVar tv) = shows tv
  showsPrec d (Forall tv ty) =
    showParen (d > forallPrec) $
    showString "∀ " . shows tv . showString ". " . showsPrec forallPrec ty
  showsPrec d (Arr l r) =
    showParen (d > arrPrec) $
    showsPrec (arrPrec + 1) l . showString " -> " . showsPrec d r
