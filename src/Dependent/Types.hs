module Dependent.Types where

import Data.List (intercalate)

data Term a =
    Type Int
  | Unit
  | UnitTy
  | Bool Bool
  | BoolTy
  | Var a
  | App (Term a) (Term a)
  | Pi a (Term a) (Term a)
  | Lambda a (Term a) (Term a)
  deriving (Functor, Foldable, Eq)

arrPrec :: Int
arrPrec = 10

appPrec :: Int
appPrec = 11

lamPrec :: Int
lamPrec = 8

groupPis :: Term a -> ([(a, Term a)], Term a)
groupPis (Pi v ty l) = let (xs, l') = groupPis l
                        in ((v, ty):xs, l')
groupPis l = ([], l)

groupLams :: Term a -> ([(a, Term a)], Term a)
groupLams (Lambda v ty l) = let (xs, l') = groupLams l
                             in ((v, ty):xs, l')
groupLams l = ([], l)

instance (Show a) => Show (Term a) where
  showsPrec _ (Var a) = shows a
  showsPrec d (App l r) = showParen (d > appPrec) $
    showsPrec appPrec l . showString " " . showsPrec (appPrec + 1) r
  showsPrec d e@Pi{} =
    let (vars, e') = groupPis e
        varList = intercalate ", " (map (\(v, t) -> show v ++ " : " ++ show t) vars)
     in showParen (d > lamPrec) $
        showString "Π" . showString varList . showString " . " . showsPrec lamPrec e'
  showsPrec d e@Lambda{} =
    let (vars, e') = groupLams e
        varList = intercalate ", " (map (\(v, t) -> show v ++ " : " ++ show t) vars)
     in showParen (d > lamPrec) $
        showString "λ" . showString varList . showString " . " . showsPrec lamPrec e'
  showsPrec _ (Bool b) = showString (if b then "true" else "false")
  showsPrec _ Unit = showString "unit"
  showsPrec _ (Type n) = showString "Type" . shows n
  showsPrec _ BoolTy = showString "Bool"
  showsPrec _ UnitTy = showString "Unit"
