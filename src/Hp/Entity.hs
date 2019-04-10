{-# LANGUAGE UndecidableInstances #-}

module Hp.Entity
  ( Entity(..)
  ) where

import Hp.IsEntity (EntityId)

-- | An entity is a value (value) paired with its persistent identity (key).
data Entity a
  = Entity
  { key :: EntityId a
  , value :: a
  } deriving stock (Generic)

deriving instance (Show a, Show (EntityId a)) => Show (Entity a)
