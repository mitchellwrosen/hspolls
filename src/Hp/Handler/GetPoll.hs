module Hp.Handler.GetPoll
  ( handleGetPoll
  ) where

import Hp.Eff.PersistPoll      (PersistPollEffect, getPoll)
import Hp.Eff.Throw            (ThrowEffect, throw)
import Hp.Entity.Poll          (PollId)
import Hp.ResponseBody.GetPoll (GetPollResponseBody(..))

import Control.Effect
import Servant        (ServerError, err404)


handleGetPoll ::
     ( Carrier sig m
     , Member PersistPollEffect sig
     , Member (ThrowEffect ServerError) sig
     )
  => PollId
  -> m GetPollResponseBody
handleGetPoll pollId =
  getPoll pollId >>= \case
    Nothing ->
      throw err404

    Just poll ->
      pure GetPollResponseBody
        { created = poll ^. #value . #created
        , duration = poll ^. #value . #duration
        , elements = poll ^. #value . #elements
        }
