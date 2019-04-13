module Hp.Subscription
  ( Subscription(..)
  ) where

data Subscription
  = Subscription
  { pollCreated :: Bool
  } deriving stock (Generic)
