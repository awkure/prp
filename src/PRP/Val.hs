module PRP.Val where

import           Data.Default
import           Data.Monoid
import           GHC.Generics (Generic)

import           PRP.Prelude

data Val (x :: Type)
  = Undefined
  | Conflict
  | Exact x
  deriving stock (Eq, Ord, Show, Functor, Generic)
  deriving (Bounded, Num) via (Ap Val x)

instance Applicative Val where
  pure = Exact

  Exact f <*> Exact x = Exact do f x

  Undefined <*> _     = Undefined
  _ <*> Undefined     = Undefined

  Conflict <*> _      = Conflict
  _ <*> Conflict      = Conflict


instance Eq content => Semigroup (Val content) where
  Conflict <> _ = Conflict
  _ <> Conflict = Conflict

  this <> Undefined = this
  Undefined <> that = that

  Exact this <> Exact that
    | this == that = Exact this
    | otherwise    = Conflict

instance Eq content => Monoid (Val content) where
  mempty = Undefined
