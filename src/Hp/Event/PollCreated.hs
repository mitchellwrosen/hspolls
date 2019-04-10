module Hp.Event.PollCreated where

import Hp.Entity (Entity)
import Hp.Poll   (Poll)


-- | A poll was created.
data PollCreatedEvent
  = PollCreatedEvent
  { poll :: Entity Poll
  } deriving stock (Generic, Show)
