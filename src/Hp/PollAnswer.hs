module Hp.PollAnswer where

import Hp.PollItemAnswer (PollItemAnswer)

import Data.Aeson (FromJSON)


newtype PollAnswer
  = PollAnswer { unPollAnswer :: Seq PollItemAnswer }
  deriving stock (Show)
  deriving newtype (FromJSON)
