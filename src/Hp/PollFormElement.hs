module Hp.PollFormElement
  ( PollFormElement(..)
  ) where

import Hp.PollQuestion (PollQuestion)

import Data.Aeson (FromJSON, ToJSON)


-- TODO proper markdown type
newtype Markdown
  = Markdown Text
  deriving newtype (FromJSON, ToJSON, Show)

data PollFormElement
  = MarkdownElement Markdown
  | QuestionElement PollQuestion
  deriving stock (Generic, Show)

-- TODO FromJSON PollFormElement
instance FromJSON PollFormElement where

-- TODO ToJSON PollFormElement
instance ToJSON PollFormElement where
