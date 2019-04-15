-- | Real database carrier for the PersistPollAnswer effect.

{-# LANGUAGE UndecidableInstances #-}

module Hp.Eff.PersistPollAnswer.DB
  ( runPersistPollAnswerDB
  ) where

import Hp.Eff.DB                (DB, runDB)
import Hp.Eff.PersistPollAnswer (PersistPollAnswerEffect(..))
import Hp.Entity                (Entity)
import Hp.Entity.Poll           (PollId)
import Hp.Entity.PollAnswer     (PollAnswer)
import Hp.Entity.User           (UserId)
import Hp.PollQuestionAnswer    (PollQuestionAnswer)

import Control.Effect
import Control.Effect.Carrier
import Control.Effect.Sum
import Hasql.Session          (Session)


newtype PersistPollAnswerCarrierDB m a
  = PersistPollAnswerCarrierDB { unPersistPollAnswerCarrierDB :: m a }
  deriving newtype (Applicative, Functor, Monad, MonadIO)

instance
     ( Carrier sig m
     , Member DB sig
     )
  => Carrier (PersistPollAnswerEffect :+: sig) (PersistPollAnswerCarrierDB m) where

  eff ::
       (PersistPollAnswerEffect :+: sig) (PersistPollAnswerCarrierDB m) (PersistPollAnswerCarrierDB m a)
    -> PersistPollAnswerCarrierDB m a
  eff = \case
    L (PutPollAnswer pollId response userId next) ->
      PersistPollAnswerCarrierDB (doPutPollAnswer pollId response userId) >>=
        next

    R other ->
      PersistPollAnswerCarrierDB (eff (handleCoercible other))

doPutPollAnswer ::
     ( Carrier sig m
     , Member DB sig
     )
  => PollId
  -> [PollQuestionAnswer]
  -> Maybe UserId
  -> m (Entity PollAnswer)
doPutPollAnswer _pollId _response _userId =
  runDB session

  where
    -- TODO insert poll answer
    session :: Session (Entity PollAnswer)
    session =
      undefined

runPersistPollAnswerDB :: PersistPollAnswerCarrierDB m a -> m a
runPersistPollAnswerDB =
  unPersistPollAnswerCarrierDB
