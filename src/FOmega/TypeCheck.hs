module FOmega.TypeCheck where

import FOmega.Types
import FOmega.Util
import Types.Variable

guard :: Bool -> a -> Either a ()
guard b s = if b then pure () else Left s

typeCheck :: (Variable tv, Variable a) => [(tv, Kind)] -> [(a, LamType tv)] -> Lam tv a -> Either String (LamType tv)
typeCheck kCxt cxt = \case
  Bool _ -> pure BoolTy
  Unit -> pure UnitTy
  Var v -> maybe (Left $ "Couldn't find variable " ++ show v) pure $ lookup v cxt
  Abs v ty e -> do
    ensureProper =<< kindCheck kCxt ty
    resultTy <- typeCheck kCxt ((v,ty):cxt) e
    pure (TArr ty resultTy)
  App l r -> do
    lType <- typeCheck kCxt cxt l
    rightTy <- typeCheck kCxt cxt r
    ensureProper =<< kindCheck kCxt lType
    ensureProper =<< kindCheck kCxt rightTy
    case lType of
      TArr argTy resultTy -> do
        guard (argTy `equiv` rightTy) $ "Argument type was " ++ show rightTy ++ " but expected " ++ show argTy
        pure resultTy
      _ -> Left $ "Got non-function type on left side of application: " ++ show lType
  AbsTy tv kind l -> Forall tv kind <$> typeCheck ((tv, kind):kCxt) cxt l -- TODO: Check if tv occurs in cxt, freshen?
  AppTy e ty -> do
    exprType <- typeCheck kCxt cxt e
    case evalType exprType of
      Forall tv _ generalType -> pure (generalType `applyTVar` (tv, ty))
      _ -> Left $ "Can't apply type to non-universal " ++ show (evalType exprType)
  ImplEx {implType, implTerm, exType} ->
    let exType' = evalType exType in
    case exType' of
      Exists exTv exKind exTy -> do
        let substType = exTy `applyTVar` (exTv, implType)
        implTermType <- typeCheck ((exTv, exKind):kCxt) cxt implTerm
        guard (substType `equiv` implTermType) $ "Existential impl type didn't match: expected " ++ show (evalType substType) ++ " but got " ++ show (evalType implTermType)
        pure exType'
      _ -> Left $ "Pack type wasn't existential: " ++ show exType'
  UseEx {letTv, letVar, letTerm, inTerm} -> do
    letTermType <- typeCheck kCxt cxt letTerm
    case evalType letTermType of
      Exists exTv exKind exType -> do
        let fixedExType = exType `applyTVar` (exTv, TVar letTv)
        checked <- typeCheck ((letTv,exKind):kCxt) ((letVar,fixedExType):cxt) inTerm
        guard (letTv `notElem` checked) $ "Type variable " ++ show letTv ++ " appeared free when unpacking existential: " ++ show checked
        pure checked
      _ -> Left $ "Tried to unpack term that wasn't packed: " ++ show (evalType letTermType)
  MkPair l r -> do
    lType <- typeCheck kCxt cxt l
    rType <- typeCheck kCxt cxt r
    ensureProper =<< kindCheck kCxt (Pair lType rType)
    pure (Pair lType rType)
  PairFirst p -> do
    pType <- typeCheck kCxt cxt p
    ensureProper =<< kindCheck kCxt pType
    case evalType pType of
      Pair lTy _ -> pure lTy
      ty -> Left $ show p ++ " : " ++ show ty ++ " wasn't a pair"
  PairSecond p -> do
    pType <- typeCheck kCxt cxt p
    ensureProper =<< kindCheck kCxt pType
    case evalType pType of
      Pair _ rTy -> pure rTy
      ty -> Left $ show p ++ " : " ++ show ty ++ " wasn't a pair"

equiv :: (Variable tv) => LamType tv -> LamType tv -> Bool
equiv l r = weakEquiv (evalType l) (evalType r)

weakEquiv :: (Variable tv) => LamType tv -> LamType tv -> Bool
weakEquiv (KApp l r) (KApp l' r')              = weakEquiv l l' && weakEquiv r r'
weakEquiv (TArr l r) (TArr l' r')              = weakEquiv l l' && weakEquiv r r'
weakEquiv (Pair l r) (Pair l' r')              = weakEquiv l l' && weakEquiv r r'
weakEquiv (Forall tv k ty) (Forall tv' k' ty') = k == k' && weakEquiv (ty `rename` (tv, tv')) ty'
weakEquiv (Exists tv k ty) (Exists tv' k' ty') = k == k' && weakEquiv (ty `rename` (tv, tv')) ty'
weakEquiv (KLam tv k ty) (KLam tv' k' ty')     = k == k' && weakEquiv (ty `rename` (tv, tv')) ty'
weakEquiv t t'                                 = t == t'

ensureProper :: Kind -> Either String ()
ensureProper kind = guard (kind == Proper) $ "Expected " ++ show kind ++ " to be " ++ show Proper

kindCheck :: (Variable tv) => [(tv, Kind)] -> LamType tv -> Either String Kind
kindCheck cxt = \case
  BoolTy -> pure Proper
  UnitTy -> pure Proper
  TVar tv -> maybe (Left $ "Couldn't find type variable " ++ show tv ++ " in context " ++ show cxt) pure $ lookup tv cxt
  TArr l r -> checkPair l r
  Pair l r -> checkPair l r
  Forall tv kind ty -> checkQuantifier tv kind ty
  Exists tv kind ty -> checkQuantifier tv kind ty
  KLam tv kind ty -> KArr kind <$> kindCheck ((tv,kind):cxt) ty
  KApp l r -> do
    leftKind <- kindCheck cxt l
    rightKind <- kindCheck cxt r
    case leftKind of
      KArr argKind resultKind -> do
        guard (argKind == rightKind) $ "Left side of type application was " ++ show argKind ++ ", expected " ++ show rightKind
        pure resultKind
      Proper -> Left "Left side of type application was proper"
  where
    checkPair l r = do
      ensureProper =<< kindCheck cxt l
      ensureProper =<< kindCheck cxt r
      pure Proper
    checkQuantifier tv kind ty = do
      ensureProper =<< kindCheck ((tv, kind):cxt) ty
      pure Proper
