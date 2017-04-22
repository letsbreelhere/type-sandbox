module LambdaOmega.Types where

data Lam tv a
  = Var a
  | App (Lam tv a) (Lam tv a)
  | Abs a (LamType tv) (Lam tv a)
  | Bool Bool
  | Unit
  deriving (Show, Functor, Foldable)

data LamType tv
  = BoolTy
  | UnitTy
  | TVar tv
  | TArr (LamType tv) (LamType tv)
  | KLam tv Kind (LamType tv)
  | KApp (LamType tv) (LamType tv)
  deriving (Eq, Functor, Foldable)

data Kind
  = Proper
  | KArr Kind Kind
  deriving (Eq)

instance (Show tv) => Show (LamType tv) where
  showsPrec _ (TVar tv) = shows tv
  showsPrec _ BoolTy = showString "Bool"
  showsPrec _ UnitTy = showString "Unit"
  showsPrec d (TArr l r) =
    showParen (d > arrPrec) $
    showsPrec (arrPrec + 1) l . showString " -> " . showsPrec d r
      where arrPrec = 10
  showsPrec d (KLam tv kind ty) =
    showParen (d > lamPrec) $
    showString "λ" . shows tv . showString " :: " . shows kind . showString " . " . shows ty
      where lamPrec = 8
  showsPrec d (KApp l r) =
    showParen (d > appPrec) $
    showsPrec (appPrec + 1) l . showString " " . showsPrec (appPrec + 1) r
      where appPrec = 11

instance Show Kind where
  showsPrec _ Proper = showString "★"
  showsPrec d (KArr l r) =
    showParen (d > arrPrec) $
      showsPrec (arrPrec + 1) l . showString " => " . showsPrec d r
    where arrPrec = 10
