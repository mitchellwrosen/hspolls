module Hp.PollQuestionAnswer
  ( PollQuestionAnswer(..)
  , arePollQuestionAnswersValid
  , isPollQuestionAnswerValid
  ) where

import Hp.PollQuestion (PollQuestion(..))

import Data.Aeson (FromJSON(..))


data PollQuestionAnswer
  = CheckboxAnswer [Bool]
  -- = RadioAnswer Natural -- 0-based index into question
  deriving stock (Show)

-- TODO instance FromJSON PollQuestionAnswer
instance FromJSON PollQuestionAnswer where
  parseJSON = undefined

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
