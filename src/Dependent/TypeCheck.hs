module Dependent.TypeCheck where

import Dependent.Types
import Types.Variable
import Util.Terms

guard :: Bool -> a -> Either a ()
guard b s = if b then pure () else Left s

eval :: Variable a => Term a -> Term a
eval (App l r) =
  case eval l of
    Lambda x _ t -> eval (substitute x (eval r) (eval t))
    l' -> App l' (eval r)
eval root = root

typeCheck :: (Variable a) => [(a, Term a)] -> Term a -> Either String (Term a)
typeCheck cxt = \case
  Type n -> pure (Type (n + 1))
  Unit -> pure UnitTy
  UnitTy -> pure (Type 0)
  Bool _ -> pure BoolTy
  BoolTy -> pure (Type 0)
  Pi v arg body -> do
    -- TODO: Do real error throwing here.
    Type k <- typeCheck cxt arg
    Type m <- typeCheck ((v,arg):cxt) body
    pure (Type (max k m))
  Var v -> maybe (Left $ "Couldn't find variable " ++ show v) pure $ lookup v cxt
  App l r -> do
    lType <- typeCheck cxt l
    rType <- typeCheck cxt r
    case lType of
      Pi v argTy resultTy -> do
        guard (argTy `equiv` rType) $ "Argument type was " ++ show rType ++ " but expected " ++ show argTy
        pure $ substitute v r resultTy
      _ -> Left $ "Got non-function type on left side of application: " ++ show lType
  Lambda v ty e -> do
    tyTy <- typeCheck cxt ty
    case tyTy of
      Type _ -> Right ()
      _ -> Left $ "Type of " ++ show ty ++ " was " ++ show tyTy ++ ", should have been Type"
    resultTy <- typeCheck ((v, ty):cxt) e
    pure (Pi v ty resultTy)

equiv :: (Variable a) => Term a -> Term a -> Bool
equiv (Lambda v ty l) (Lambda v' ty' r) =
  let r' = rename r (v', v)
      ty'' = rename ty' (v', v)
   in ty `equiv` ty'' && l `equiv` r'
equiv (Pi v ty l) (Pi v' ty' r) =
  let r' = rename r (v', v)
      ty'' = rename ty' (v', v)
   in ty `equiv` ty'' && l `equiv` r'
equiv l r = eval l == eval r

substitute :: (Variable a) => a -> Term a -> Term a -> Term a
substitute x r = \case
  Var x'
    | x == x' -> r
    | otherwise -> Var x'
  App s t -> App (substitute x r s) (substitute x r t)
  Lambda y ty t
    | x == y -> Lambda x (substitute x r ty) t
    | otherwise -> let (freshVar, t') = freshen r t y
                    in Lambda freshVar (substitute x r ty) (substitute x r t')
  Pi y ty t
    | x == y -> Pi x (substitute x r ty) t
    | otherwise -> let (freshVar, t') = freshen r t y
                    in Pi freshVar (substitute x r ty) (substitute x r t')
  root -> root
