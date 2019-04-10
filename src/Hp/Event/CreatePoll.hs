module Hp.Event.CreatePoll where

import Hp.Entity (Entity)
import Hp.Poll   (Poll)


-- | A poll was created.
data CreatePollEvent
  = CreatePollEvent
  { poll :: Entity Poll
  } deriving stock (Generic, Show)
