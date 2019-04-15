module Hp.ResponseBody.GetPoll
  ( GetPollResponseBody(..)
  , makeGetPollResponseBody
  ) where

import Hp.Entity.Poll        (Poll)
import Hp.PollFormElement    (PollFormElement)
import Hp.PollQuestionAnswer (PollQuestionAnswer)

import Data.Aeson (ToJSON)
import Data.Time  (DiffTime, UTCTime)


data GetPollResponseBody
  = GetPollResponseBody
  { created :: UTCTime
  , duration :: DiffTime
  , poll :: [PollFormElement]
  , answers :: Vector [PollQuestionAnswer]
  } deriving stock (Generic)
    deriving anyclass (ToJSON)

makeGetPollResponseBody ::
     Poll
  -> Vector [PollQuestionAnswer]
  -> GetPollResponseBody
makeGetPollResponseBody poll answers =
  GetPollResponseBody
    { created = poll ^. #created
    , duration = poll ^. #duration
    , poll = poll ^. #elements
    , answers = answers
    }
