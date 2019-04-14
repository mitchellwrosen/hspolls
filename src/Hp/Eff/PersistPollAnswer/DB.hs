-- | Real database carrier for the PersistPollAnswer effect.

{-# LANGUAGE UndecidableInstances #-}

module Hp.Eff.PersistPollAnswer.DB
  ( runPersistPollAnswerDB
  ) where

import Hp.Eff.DB                (DB, runDB)
import Hp.Eff.PersistPollAnswer (PersistPollAnswerEffect(..))
import Hp.PollAnswer            (PollAnswer)
import Hp.PollAnswerId          (PollAnswerId)

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
    L (PutPollAnswer pollAnswer next) ->
      PersistPollAnswerCarrierDB (doPutPollAnswer pollAnswer) >>= next

    R other ->
      PersistPollAnswerCarrierDB (eff (handleCoercible other))

doPutPollAnswer ::
     ( Carrier sig m
     , Member DB sig
     )
  => PollAnswer
  -> m (Maybe PollAnswerId)
doPutPollAnswer _pollAnswer =
  Just <$> runDB session

  where
    -- TODO insert poll answer
    session :: Session PollAnswerId
    session =
      undefined

runPersistPollAnswerDB :: PersistPollAnswerCarrierDB m a -> m a
runPersistPollAnswerDB =
  unPersistPollAnswerCarrierDB
