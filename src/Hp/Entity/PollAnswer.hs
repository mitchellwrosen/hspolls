-- TODO PollAnswer -> PollResponse everywhere

module Hp.Entity.PollAnswer
  ( PollAnswer(..)
  , EntityId(PollAnswerId)
  , PollAnswerId
  ) where

import Hp.Entity.Poll        (PollId)
import Hp.Entity.User        (UserId)
import Hp.IsEntity           (IsEntity(..))
import Hp.PollQuestionAnswer (PollQuestionAnswer)

import Data.UUID (UUID)


data PollAnswer
  = PollAnswer
  { answers :: Seq PollQuestionAnswer
  , pollId :: PollId
  , userId :: Maybe UserId
  } deriving stock (Generic, Show)

instance IsEntity PollAnswer where
  newtype EntityId PollAnswer
    = PollAnswerId { unPollAnswerId :: UUID }
    deriving stock (Show)

type PollAnswerId
  = EntityId PollAnswer
