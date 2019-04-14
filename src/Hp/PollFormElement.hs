module Hp.PollFormElement
  ( PollFormElement(..)
  ) where

import Hp.PollQuestion (PollQuestion(..))

import Data.Aeson       (FromJSON(..), ToJSON(..), Value(..), object,
                         withObject, withText, (.:), (.=))
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

          s ->
            fail ("Unknown type: " ++ s ^. unpacked)
        )
        type_

    where
      parseCheckboxElement :: Value -> Parser PollFormElement
      parseCheckboxElement =
        withObject "checkbox" $ \o ->
          QuestionElement <$>
            (CheckboxQuestion
              <$> o .: "header"
              <*> o .: "options")

      parseMarkdownElement :: Value -> Parser PollFormElement
      parseMarkdownElement value =
        MarkdownElement <$> parseJSON value

instance ToJSON PollFormElement where
  toJSON :: PollFormElement -> Value
  toJSON = \case
    MarkdownElement markdown ->
      object
        [ "type" .= String "markdown"
        , "value" .= markdown
        ]

    QuestionElement (CheckboxQuestion header options) ->
      object
        [ "type" .= String "checkbox"
        , "value" .=
            object
              [ "header" .= header
              , "options" .= options
              ]
        ]
