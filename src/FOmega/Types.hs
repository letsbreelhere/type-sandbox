module FOmega.Types where

data Lam tv a
  = Var a
  | App (Lam tv a) (Lam tv a)
  | Abs a (LamType tv) (Lam tv a)
  | Bool Bool
  | Unit
  | AbsTy tv Kind (Lam tv a)
  | AppTy (Lam tv a) (LamType tv)
  | UseEx
      { letTv :: tv
      , letVar :: a
      , letTerm :: Lam tv a
      , inTerm :: Lam tv a
      }
  | ImplEx
      { implType :: LamType tv
      , implTerm :: Lam tv a
      , exType :: LamType tv
      }
  | MkPair (Lam tv a) (Lam tv a)
  | PairFirst (Lam tv a)
  | PairSecond (Lam tv a)
  deriving (Functor, Foldable)

data LamType tv
  = BoolTy
  | UnitTy
  | TVar tv
  | TArr (LamType tv) (LamType tv)
  | Forall tv Kind (LamType tv)
  | Exists tv Kind (LamType tv)
  | KLam tv Kind (LamType tv)
  | KApp (LamType tv) (LamType tv)
  | Pair (LamType tv) (LamType tv)
  deriving (Eq, Functor, Foldable)

data Kind
  = Proper
  | KArr Kind Kind
  deriving (Eq)

arrPrec :: Int
arrPrec = 10

forallPrec :: Int
forallPrec = 8

appPrec :: Int
appPrec = 11

lamPrec :: Int
lamPrec = 8

groupLams :: Lam tv a -> ([a], Lam tv a)
groupLams (Abs v _ l) = let (xs, l') = groupLams l
                          in (v:xs, l')
groupLams l = ([], l)

groupBigLams :: Lam tv a -> ([tv], Lam tv a)
groupBigLams (AbsTy tv _ l) = let (xs, l') = groupBigLams l
                               in (tv:xs, l')
groupBigLams l = ([], l)

instance (Show tv, Show a) => Show (Lam tv a) where
  showsPrec _ (Var a) = shows a
  showsPrec d (App l r) = showParen (d > appPrec) $
    showsPrec appPrec l . showString " " . showsPrec (appPrec + 1) r
  showsPrec d e@Abs{} =
    let (vars, e') = groupLams e
        varList = unwords (map show vars)
     in showParen (d > lamPrec) $
        showString "λ" . showString varList . showString " . " . showsPrec lamPrec e'
  showsPrec _ (Bool b) = showString (if b then "true" else "false")
  showsPrec _ Unit = showString "unit"
  showsPrec d e@AbsTy{} =
    let (tvs, e') = groupBigLams e
        tvList = unwords (map show tvs)
     in showParen (d > lamPrec) $
       showString "Λ" . showString tvList . showString " . " . showsPrec lamPrec e'
  showsPrec d (AppTy l ty) = showsPrec d l . showString " [" . shows ty . showString "]"
  showsPrec d UseEx {letTv, letVar, letTerm, inTerm} =
    showParen (d > lamPrec) $
    showString "let {" .
      shows letTv .  showString ", " . shows letVar .
    showString "}=" .
    shows letTerm .
    showString " in " .
    shows inTerm
  showsPrec _ (ImplEx ty l ty') = showString "{*" . shows ty . showString "," . shows l . showString "} as " . shows ty'
  showsPrec _ (MkPair l r) = showString "⟨" . shows l . showString ", " . shows r . showString "⟩"
  showsPrec d (PairFirst l)  = showParen (d > appPrec) $ showString "fst " . showsPrec (appPrec + 1) l
  showsPrec d (PairSecond l) = showParen (d > appPrec) $ showString "snd " . showsPrec (appPrec + 1) l

groupForalls :: LamType tv -> ([tv], LamType tv)
groupForalls (Forall tv _ ty) = let (xs, ty') = groupForalls ty
                                 in (tv:xs, ty')
groupForalls ty = ([], ty)

instance (Show tv) => Show (LamType tv) where
  showsPrec d ty@Forall{} =
    let (tvs, ty') = groupForalls ty
        tvList = unwords (map show tvs)
     in showParen (d > forallPrec) $
        showString "∀ " . showString tvList . showString " . " . showsPrec forallPrec ty'
  showsPrec _ (TVar tv) = shows tv
  showsPrec _ BoolTy = showString "Bool"
  showsPrec _ UnitTy = showString "Unit"
  showsPrec d (TArr l r) =
    showParen (d > arrPrec) $
    showsPrec (arrPrec + 1) l . showString " -> " . shows r
  showsPrec d (KLam tv _ ty) =
    showParen (d > lamPrec) $
    showString "λ" . shows tv . showString " . " . showsPrec lamPrec ty
  showsPrec d (KApp l r) =
    showParen (d > appPrec) $
    showsPrec appPrec l . showString " " . showsPrec (appPrec + 1) r
  showsPrec d (Exists tv _ ty) =
    showParen (d > forallPrec) $
      showString "∃" . shows tv . showString " . " . showsPrec forallPrec ty
  showsPrec _ (Pair l r) = showString "⟨" . shows l . showString ", " . shows r . showString "⟩"

instance Show Kind where
  showsPrec _ Proper = showString "⭑"
  showsPrec d (KArr l r) =
    showParen (d > arrPrec) $
      showsPrec (arrPrec + 1) l . showString " => " . showsPrec arrPrec r
