module FOmega.Examples where

import FOmega.Types
import Types.Name

pi_ :: TyName
pi_ = TyName 15

tau :: TyName
tau = TyName 19

-- A convoluted way of proving `x` is to prove `∃ a . ⟨a, a -> x⟩`.
convolutedImpl :: Lam TyName Name
convolutedImpl =
  let mpTerm = MkPair Unit (Abs "u" UnitTy (Bool True))
   in ImplEx
     { implType = UnitTy
     , implTerm = mpTerm
     , exTv = tau
     , exKind = Proper
     , exType = Pair (TVar tau) (TArr (TVar tau) BoolTy)
     }

-- Using the implementation above, we can apply the provided terms to obtain a Bool.
convolutedUse :: Lam TyName Name
convolutedUse = UseEx
  { letTv = tau
  , letVar = "p"
  , letTerm = convolutedImpl
  , inTerm = PairSecond (Var "p") `App` PairFirst (Var "p")
  }

-- A non-well-typed use of the existential above, since its type variable escapes capture.
convolutedUseInvalid :: Lam TyName Name
convolutedUseInvalid = UseEx
  { letTv = tau
  , letVar = "p"
  , letTerm = convolutedImpl
  , inTerm = PairFirst (Var "p")
  }

pairImpl :: Lam TyName Name
pairImpl =
  let pairTy = TVar pi_ `KApp` TVar alpha `KApp` TVar beta
      mkPairTy = Forall alpha Proper (Forall beta Proper (TVar alpha `TArr` (TVar beta `TArr` pairTy)))
      firstTy = Forall alpha Proper (Forall beta Proper (pairTy `TArr` TVar alpha))
      mkPair =
        AbsTy alpha Proper (
          AbsTy beta Proper (
            Abs "x" (TVar alpha) (
              Abs "y" (TVar beta) (
                MkPair (Var "x") (Var "y")
              )
            )
          )
        )
      first =
        AbsTy alpha Proper (
          AbsTy beta Proper (
            Abs "p" (Pair (TVar alpha) (TVar beta)) (PairFirst (Var "p"))
          )
        )
  in ImplEx
    { implType = KLam alpha Proper (KLam beta Proper (Pair (TVar alpha) (TVar beta)))
    , implTerm = MkPair mkPair first
    , exTv = pi_
    , exKind = Proper `KArr` (Proper `KArr` Proper)
    , exType = Pair mkPairTy firstTy
    }

pairUse :: Lam TyName Name
pairUse =
  let madePair = PairFirst (Var "v") `AppTy` BoolTy `AppTy` BoolTy `App` Bool True `App` Bool False
      usedFirst = PairSecond (Var "v") `AppTy` BoolTy `AppTy` BoolTy
   in UseEx
  { letTv = tau
  , letVar = "v"
  , letTerm = pairImpl
  , inTerm = usedFirst `App` madePair
  }
