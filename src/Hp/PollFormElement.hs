module Hp.PollFormElement
  ( PollFormElement(..)
  , arePollFormElementsValid
  ) where

import Hp.PollQuestion (PollQuestion(..), isPollQuestionValid)

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

-- | Validate a list of form elements:
--
-- * There's at least one question
-- * There aren't two markdown elements in a row
-- * Elements are all individually valid
arePollFormElementsValid :: [PollFormElement] -> Bool
arePollFormElementsValid elements =
  and
    [ atLeastOneQuestion
    , notTwoConsecutiveMarkdown
    , all isPollFormElementValid elements
    ]

  where
    atLeastOneQuestion :: Bool
    atLeastOneQuestion =
      not (null [ () | QuestionElement{} <- elements ])

    notTwoConsecutiveMarkdown :: Bool
    notTwoConsecutiveMarkdown =
      null
        [ () | (MarkdownElement{}, MarkdownElement{}) <- consecutive elements ]
      where
        consecutive :: [a] -> [(a, a)]
        consecutive = \case
          [] -> []
          _:[] -> []
          x:y:ys -> (x, y) : consecutive (y:ys)

-- | Validate a single form element
isPollFormElementValid :: PollFormElement -> Bool
isPollFormElementValid = \case
  MarkdownElement markdown ->
    isMarkdownValid markdown
  QuestionElement question ->
    isPollQuestionValid question

isMarkdownValid :: Markdown -> Bool
isMarkdownValid _ = True -- TODO validate markdown
