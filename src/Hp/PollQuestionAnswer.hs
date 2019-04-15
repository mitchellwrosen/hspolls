module Hp.PollQuestionAnswer
  ( PollQuestionAnswer(..)
  , isValidPollQuestionAnswer
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

-- | Does this question/answer pair make sense?
--
-- Precondition: question was already validated with isValidPollQuestion
--
-- TODO gdp
isValidPollQuestionAnswer :: PollQuestion -> PollQuestionAnswer -> Bool
isValidPollQuestionAnswer question answer =
  case (question, answer) of
    (CheckboxQuestion _ xs, CheckboxAnswer ys) ->
      length xs == length ys
