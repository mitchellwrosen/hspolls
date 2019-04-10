module Hp.PollQuestionAnswer where

import Data.Aeson (FromJSON(..))


data PollQuestionAnswer
  = RadioAnswer Natural -- 0-based index into question
  deriving stock (Show)

-- TODO instance FromJSON PollQuestionAnswer
instance FromJSON PollQuestionAnswer where
  parseJSON = undefined
