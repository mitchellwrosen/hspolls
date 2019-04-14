module Hp.PollFormElement
  ( PollFormElement(..)
  ) where

import Hp.PollQuestion (PollQuestion)

import Data.Aeson       ((.:), FromJSON(..), ToJSON(..), Value, withObject, withText)
import Data.Aeson.Types (Parser)


-- TODO proper markdown type
newtype Markdown
  = Markdown Text
  deriving newtype (FromJSON, Show, ToJSON)

data PollFormElement
  = MarkdownElement Markdown
  | QuestionElement PollQuestion
  deriving stock (Generic, Show)

instance FromJSON PollFormElement where
  parseJSON :: Value -> Parser PollFormElement
  parseJSON =
    withObject "PollFormElement" $ \o -> do
      type_ <- o .: "type"
      value <- o .: "value"

      withText "type"
        (\case
          "checkbox" ->
            parseCheckboxElement value

          "markdown" ->
            parseMarkdownElement value

          _ ->
            undefined
        )
        type_

    where
      parseCheckboxElement :: Value -> Parser PollFormElement
      parseCheckboxElement value =
        undefined

      parseMarkdownElement :: Value -> Parser PollFormElement
      parseMarkdownElement value =
        MarkdownElement <$> parseJSON value

instance ToJSON PollFormElement where
  toJSON = undefined
