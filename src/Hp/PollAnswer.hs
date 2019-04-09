module Hp.PollAnswer where

newtype PollAnswer
  = PollAnswer { unPollAnswer :: Seq PollItemAnswer }
