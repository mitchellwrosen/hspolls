module Hp.PollQuestion where

import Data.Aeson (FromJSON, ToJSON)


-- | A single question in a poll.
data PollQuestion
  = CheckboxQuestion Text Bool
  | DropdownQuestion Text [Text]
  | NumberQuestion Text Int
  | RadioQuestion Text [Text]
  | TextQuestion Text Bool Text -- Bool means: is text area?
  deriving stock (Generic, Show)

-- TODO instance From/ToJSON PollQuestion
instance FromJSON PollQuestion where
instance ToJSON PollQuestion where
