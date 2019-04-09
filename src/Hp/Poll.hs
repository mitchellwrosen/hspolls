module Hp.Poll
  ( Poll(..)
  , PollId(..)
  ) where

import Hp.PollFormElement

import Data.Aeson      (FromJSON)
import Data.Time       (UTCTime)
import Data.UUID       (UUID)
import Web.HttpApiData (FromHttpApiData)


newtype PollId
  = PollId { unPollId :: UUID }
  deriving stock (Show)
  deriving newtype (FromHttpApiData, FromJSON)

data Poll
  = Poll
  { elements :: Seq PollFormElement
  , endTime :: UTCTime
  } deriving stock (Generic, Show)
    deriving anyclass (FromJSON)
