-- TODO PollAnswer -> PollResponse everywhere

module Hp.PollAnswer
  ( PollAnswer(..)
  ) where

import Hp.IsEntity           (IsEntity(..))
import Hp.PollAnswerId       (PollAnswerId)
import Hp.PollId             (PollId)
import Hp.PollQuestionAnswer (PollQuestionAnswer)
import Hp.UserId             (UserId)


data PollAnswer
  = PollAnswer
  { answers :: Seq PollQuestionAnswer
  , pollId :: PollId
  , userId :: Maybe UserId
  } deriving stock (Generic, Show)

instance IsEntity PollAnswer where
  type EntityId PollAnswer
    = PollAnswerId
