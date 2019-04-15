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
-- * It's not empty
-- * Markdown is valid (i.e. not empty)
-- * Questions are valid
-- * There aren't two markdown elements in a row
arePollFormElementsValid :: Seq PollFormElement -> Bool
arePollFormElementsValid elements =
  case uncons elements of
    Nothing ->
      False

    Just (elem0, elems) ->
      and
        [ isPollFormElementValid elem0
        , go elem0 elems
        ]

  where
    go :: PollFormElement -> Seq PollFormElement -> Bool
    go x xs =
      case uncons xs of
        Nothing ->
          True

        Just (y, ys) ->
          and
            [ isPollFormElementValid y
            , case (x, y) of
                (MarkdownElement{}, MarkdownElement{}) -> False
                _ -> True
            , go y ys
            ]

-- | Validate a single form element:
--
-- * Markdown isn't empty
-- * Question is valid
isPollFormElementValid :: PollFormElement -> Bool
isPollFormElementValid = \case
  MarkdownElement markdown ->
    isMarkdownValid markdown
  QuestionElement question ->
    isPollQuestionValid question

isMarkdownValid :: Markdown -> Bool
isMarkdownValid =
  undefined
