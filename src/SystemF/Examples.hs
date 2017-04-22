module SystemF.Examples where

import SystemF.Types
import Types.Name

-- The identity function, λx. x : ∀ a. a -> a
ident :: Lam TyName Name
ident = AbsTy "α" (Abs "x" (TVar "α") (Var "x"))

-- The application function, λf. λx. f x : ∀ a. ∀ b. (a -> b) -> a -> b
apply :: Lam TyName Name
apply =
  AbsTy "α" (
    AbsTy "β" (
      Abs "f" (Arr (TVar "α") (TVar "β")) (
        Abs "x" (TVar "α") (
          App (Var "f") (Var "x")
        )
      )
    )
  )

-- Applying identity to true:
-- (λf. λx. f x) (λx. x) true : bool
applied :: Lam TyName Name
applied = App (App (AbsTy "γ" (apply `AppTy` TVar "γ" `AppTy` TVar "γ") `AppTy` BoolTy) (AppTy ident BoolTy)) (Bool True)
