module Hp.PollId
  ( PollId(..)
  ) where

import Data.Aeson      (FromJSON)
import Data.UUID       (UUID)
import Web.HttpApiData (FromHttpApiData)


newtype PollId
  = PollId { unPollId :: UUID }
  deriving stock (Show)
  deriving newtype (FromHttpApiData, FromJSON)
