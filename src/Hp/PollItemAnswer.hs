module Hp.PollItemAnswer where

import Data.Aeson (FromJSON(..))


data PollItemAnswer
  = RadioAnswer Natural -- 0-based index into question

-- TODO instance FromJSON PollItemAnswer
instance FromJSON PollItemAnswer where
  parseJSON = undefined
