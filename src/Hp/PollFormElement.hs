module Hp.PollFormElement where

import Data.Aeson (FromJSON)


-- TODO proper markdown type
newtype Markdown
  = Markdown Text
  deriving newtype (FromJSON)

-- TODO checkbox
-- TODO text field
-- TODO text area
-- TODO dropdown
-- TODO numeric input
data PollFormElement
  = MarkdownElement Markdown
  | RadioElement Text [Text]
  deriving stock (Generic)

-- TODO FromJSON PollFormElement
instance FromJSON PollFormElement where
