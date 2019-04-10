module Hp.IsEntity where

-- | The class of types with a persistent identity.
class IsEntity (a :: Type) where
  type EntityId a :: Type
