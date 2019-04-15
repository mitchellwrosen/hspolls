-- TODO PollAnswer -> PollResponse everywhere

module Hp.Entity.PollAnswer
  ( PollAnswer(..)
  , EntityId(PollAnswerId)
  , PollAnswerId
  , pollAnswerIdDecoder
  ) where

import Hp.Entity.Poll        (PollId)
import Hp.Entity.User        (UserId)
import Hp.IsEntity           (IsEntity(..))
import Hp.PollQuestionAnswer (PollQuestionAnswer)

import Data.Time (UTCTime)
import Data.UUID (UUID)

import qualified Hasql.Decoders as Decoder


data PollAnswer
  = PollAnswer
  { answers :: [PollQuestionAnswer]
  , created :: UTCTime
  , pollId :: PollId
  , userId :: Maybe UserId
  } deriving stock (Generic, Show)

instance IsEntity PollAnswer where
  newtype EntityId PollAnswer
    = PollAnswerId { unPollAnswerId :: UUID }
    deriving stock (Show)

type PollAnswerId
  = EntityId PollAnswer

pollAnswerIdDecoder :: Decoder.Value PollAnswerId
pollAnswerIdDecoder =
  PollAnswerId <$> Decoder.uuid
