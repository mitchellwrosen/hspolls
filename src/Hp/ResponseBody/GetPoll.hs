module Hp.ResponseBody.GetPoll
  ( GetPollResponseBody(..)
  ) where

import Hp.PollFormElement (PollFormElement)

import Data.Aeson (ToJSON)
import Data.Time  (DiffTime, UTCTime)


data GetPollResponseBody
  = GetPollResponseBody
  { created :: UTCTime
  , elements :: [PollFormElement]
  , duration :: DiffTime
  } deriving stock (Generic)
    deriving anyclass (ToJSON)
