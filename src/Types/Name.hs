module Types.Name where

import Control.Monad (replicateM)
import Data.Maybe (fromMaybe)
import Data.Char (toUpper, toLower)
import Data.List (elemIndex)
import Data.String

newtype CapName = CapName Int
  deriving (Eq, Ord)

instance Enum CapName where
  toEnum = CapName
  fromEnum (CapName n) = n

instance IsString CapName where
  fromString n = fromMaybe (error "fromString") (CapName <$> elemIndex (map toLower n) names)

instance Show CapName where
  show (CapName n) = map toUpper (names !! n)

newtype Name = Name Int
  deriving (Eq, Ord)

names :: [String]
names = concatMap (`replicateM` (['a'..'z'] ++ "_")) [1..]

instance Enum Name where
  toEnum = Name
  fromEnum (Name n) = n

instance IsString Name where
  fromString n = fromMaybe (error "fromString") (Name <$> elemIndex n names)

instance Show Name where
  show (Name n) = names !! n

newtype TyName = TyName Int
  deriving (Eq, Ord)

tyNames :: [String]
tyNames = concatMap (`replicateM` ['α'..'ω']) [1..]

instance Enum TyName where
  toEnum = TyName
  fromEnum (TyName n) = n

instance IsString TyName where
  fromString n = fromMaybe (error "fromString") (TyName <$> elemIndex n tyNames)

instance Show TyName where
  show (TyName n) = tyNames !! n

alpha :: TyName
alpha = TyName 0

beta :: TyName
beta = TyName 1

gamma :: TyName
gamma = TyName 2
