module Hp.Handler.CreatePoll
  ( handleCreatePoll
  ) where

import Hp.Eff.ManagePoll (ManagePoll, savePoll)
import Hp.Poll           (Poll)

import Control.Effect
import Servant        (NoContent(..))


handleCreatePoll ::
     ( Carrier sig m
     , Member ManagePoll sig
     , MonadIO m
     )
  => Poll
  -> m NoContent
handleCreatePoll poll = do
  _ <- savePoll poll
  pure NoContent
