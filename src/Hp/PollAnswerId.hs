module Hp.PollAnswerId where

import Data.UUID (UUID)


newtype PollAnswerId
  = PollAnswerId { unPollAnswerId :: UUID }
  deriving stock (Show)
