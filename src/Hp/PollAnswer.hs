-- TODO PollAnswer -> PollResponse everywhere

module Hp.PollAnswer
  ( PollAnswer(..)
  ) where

import Hp.IsEntity       (IsEntity(..))
import Hp.PollAnswerId   (PollAnswerId)
import Hp.PollId         (PollId)
import Hp.PollItemAnswer (PollItemAnswer)
import Hp.UserId         (UserId)


data PollAnswer
  = PollAnswer
  { answers :: Seq PollItemAnswer
  , pollId :: PollId
  , userId :: Maybe UserId
  } deriving stock (Generic, Show)

instance IsEntity PollAnswer where
  type EntityId PollAnswer
    = PollAnswerId
