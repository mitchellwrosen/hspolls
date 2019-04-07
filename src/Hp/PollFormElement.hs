module Hp.PollFormElement where

import Data.Aeson (FromJSON, ToJSON)


-- TODO proper markdown type
newtype Markdown
  = Markdown Text
  deriving newtype (FromJSON, ToJSON, Show)

-- TODO checkbox
-- TODO text field
-- TODO text area
-- TODO dropdown
-- TODO numeric input
data PollFormElement
  = MarkdownElement Markdown
  | RadioElement Text [Text]
  deriving stock (Generic, Show)

-- TODO FromJSON PollFormElement
instance FromJSON PollFormElement where

-- TODO ToJSON PollFormElement
instance ToJSON PollFormElement where
