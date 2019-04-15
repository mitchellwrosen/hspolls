module Hp.PollQuestionAnswer
  ( PollQuestionAnswer(..)
  , arePollQuestionAnswersValid
  , isPollQuestionAnswerValid
  ) where

import Hp.PollQuestion (PollQuestion(..))

import Data.Aeson       (FromJSON(..), ToJSON(..), Value, object, withObject,
                         withText, (.:), (.=))
import Data.Aeson.Types (Parser)


data PollQuestionAnswer
  = CheckboxAnswer [Bool]
  -- = RadioAnswer Natural -- 0-based index into question
  deriving stock (Show)

instance FromJSON PollQuestionAnswer where
  parseJSON :: Value -> Parser PollQuestionAnswer
  parseJSON =
    withObject "PollQuestionAnswer" $ \o -> do
      type_ <- o .: "type"
      value <- o .: "value"

      withText
        "type"
        (\case
          "checkbox" ->
            parseCheckboxAnswer value

          s ->
            fail ("Unknown type: " ++ s ^. unpacked)
        )
        type_

    where
      parseCheckboxAnswer :: Value -> Parser PollQuestionAnswer
      parseCheckboxAnswer value =
        CheckboxAnswer <$> parseJSON value

instance ToJSON PollQuestionAnswer where
  toJSON :: PollQuestionAnswer -> Value
  toJSON = \case
    CheckboxAnswer answers ->
      object
        [ "type" .= ("checkbox" :: Text)
        , "value" .= toJSON answers
        ]

arePollQuestionAnswersValid ::
     [PollQuestion]
  -> [PollQuestionAnswer]
  -> Bool
arePollQuestionAnswersValid questions answers =
  all (uncurry isPollQuestionAnswerValid) (zip questions answers)

-- | Does this question/answer pair make sense?
--
-- Precondition: question was already validated with isValidPollQuestion
--
-- TODO gdp
isPollQuestionAnswerValid :: PollQuestion -> PollQuestionAnswer -> Bool
isPollQuestionAnswerValid question answer =
  case (question, answer) of
    (CheckboxQuestion _ xs, CheckboxAnswer ys) ->
      length xs == length ys
