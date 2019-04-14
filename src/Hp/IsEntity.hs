module Hp.IsEntity
  ( IsEntity(..)
  ) where

-- | The class of types with a persistent identity.
class IsEntity (a :: Type) where
  data EntityId a :: Type
