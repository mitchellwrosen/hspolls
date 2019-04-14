module Hp.Event.PollCreated
  ( PollCreatedEvent(..)
  ) where

import Hp.Entity      (Entity)
import Hp.Entity.Poll (Poll)


-- | A poll was created.
data PollCreatedEvent
  = PollCreatedEvent
  { poll :: Entity Poll
  } deriving stock (Generic, Show)
